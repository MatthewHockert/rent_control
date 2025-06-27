library(dplyr)
library(ggplot2)

folder_path <- "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region"

read_permit_file <- function(path) {
  lines <- readLines(path, n = 2)
  
  # Combine headers: use header2 where header1 is blank
  header1 <- strsplit(lines[1], ",")[[1]]
  header2 <- strsplit(lines[2], ",")[[1]]
  merged_headers <- ifelse(header1 == "", header2, paste0(header1, "_", header2))
  merged_headers <- make.names(merged_headers, unique = TRUE)
  merged_headers <- gsub("[^a-zA-Z0-9]+", "_", merged_headers)
  merged_headers <- gsub("_+", "_", merged_headers)
  merged_headers <- gsub("_$", "", merged_headers)
  
  df <- read.csv(path, skip = 2, header = FALSE, stringsAsFactors = FALSE)
  if ("Zip_Code" %in% names(df)) df$Zip_Code <- as.character(df$Zip_Code)
  # Pad headers if too short
  if (length(merged_headers) < ncol(df)) {
    merged_headers <- c(merged_headers, paste0("Extra_", seq_len(ncol(df) - length(merged_headers))))
  }
  names(df) <- merged_headers[seq_len(ncol(df))]
  
  return(df)
}

# Load files
files <- list.files(folder_path, pattern = "(y)\\.txt$", full.names = TRUE)
nj_data_list_month <- lapply(files, read_permit_file)

# Get union of all column names
all_cols <- unique(unlist(lapply(nj_data_list_month, names)))

# Standardize columns across all files
# nj_data_list <- lapply(nj_data_list, function(df) {
#   for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
#   df <- df[all_cols]
#   return(df)
# })

nj_data_list_month <- lapply(nj_data_list_month, function(df) {
  for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
  df <- df[all_cols]
  
  if ("Zip_Code" %in% names(df)) df$Zip_Code <- as.character(df$Zip_Code)
  if ("Survey_Date" %in% names(df)) df$Survey_Date <- as.character(df$Survey_Date)
  
  return(df)
})


# Final bind and cleanup
nj_all_month <- bind_rows(nj_data_list_month)
beep()
colSums(is.na(nj_all_month))



# Optional fixes for character formatting
# nj_all2 <- nj_all %>%
#   mutate(
#     Zip_Code = as.character(Zip_Code),
#     Survey_Date = as.character(Survey_Date),
#     Date = as.Date(sapply(Survey_Date, parse_survey_date), origin = "1970-01-01"),
#     Year = format(Date, "%Y")
#   )
nj_all_month <- filter(nj_all_month, State_Code == "34")
colSums(is.na(nj_all_month))


print(unique(nj_all_month$Survey_Date))

parse_survey_date <- function(x) {
  if (nchar(x) == 6 && grepl("^\\d{6}$", x)) {
    year <- as.numeric(substr(x, 1, 4))
    month <- as.numeric(substr(x, 5, 6))
  } else if (nchar(x) == 4 && grepl("^\\d{4}$", x)) {
    year_prefix <- substr(x, 1, 2)
    year <- ifelse(as.numeric(year_prefix) >= 80,
                   as.numeric(paste0("19", year_prefix)),
                   as.numeric(paste0("20", year_prefix)))
    month <- as.numeric(substr(x, 3, 4))
  } else {
    year <- NA
    month <- NA
  }
  return(list(year = year, month = month))
}

parsed_dates <- lapply(nj_all_month$Survey_Date, parse_survey_date)
parsed_df <- do.call(rbind, lapply(parsed_dates, function(x) data.frame(Year = x$year, Month = x$month)))
nj_all_month2 <- bind_cols(nj_all_month, parsed_df)

library(zoo)

nj_all_month2 <- nj_all_month2 %>%
  mutate(
    ym = as.yearmon(paste(Year, Month), "%Y %m")
  )

print(unique(nj_all_month2$ym))
colSums(is.na(nj_all_month2))

beep()

print(unique(nj_all_month2$Place_Name))


clean_place_name <- function(x) {
  x <- gsub("\\(.*?\\)", "", x)
  x <- gsub("\\.+", "", x)
  x <- gsub("#", "", x)
  x <- stringr::str_to_lower(x)
  x <- trimws(x)
  x <- gsub("\\btwp\\b", "township", x)
  x <- gsub("\\bboro\\b", "borough", x)
  x <- gsub("\\bvil\\b", "village", x)
  x <- gsub("\\bcity of ", "", x)
  x <- gsub("\\btownship township\\b", "township", x)
  x <- gsub("\\bborough borough\\b", "borough", x)
  x <- gsub("\\btown town\\b", "town", x)
  x <- gsub("\\bvillage village\\b", "village", x)
  x <- gsub("\\s+", " ", x)
  return(x)
}

nj_all_month2 <- nj_all_month2 %>%
  mutate(
    Place_Name_Clean = clean_place_name(Place_Name)
  )

nrow(nj_all_month2)
nj_allx_month <- merge(nj_all_month2,nj_county_city_crosswalk,by="County_Code")
nrow(nj_allx_month)


monthly_trimmed <- nj_allx_month %>%
  select(Survey_Date, State_Code, Place_Name_Clean, County_Code, County_Name, Year, ym,
         X1_unit_Units, X2_units_Units, X3_4_units_Units, X5_units_Units)

monthly_trimmed <- monthly_trimmed %>%
  rename(
    sf_units = X1_unit_Units,
    two_units = X2_units_Units,
    three_four_units = X3_4_units_Units,
    mf_units = X5_units_Units
  )

monthly_trimmed <- monthly_trimmed %>% filter(!is.na(Date))
monthly_trimmed <- monthly_trimmed %>%
  mutate(
    single_family = as.numeric(sf_units),
    multi_family = rowSums(
      cbind(
        as.numeric(two_units),
        as.numeric(three_four_units),
        as.numeric(mf_units)
      ),
      na.rm = TRUE
    )
  )

monthly_trimmed <- monthly_trimmed %>%
  mutate(
    Survey_Date = as.character(Survey_Date),
    year = ifelse(nchar(Survey_Date) == 6,
                  substr(Survey_Date, 1, 4),
                  paste0("19", substr(Survey_Date, 1, 2))),
    month = ifelse(nchar(Survey_Date) == 6,
                   substr(Survey_Date, 5, 6),
                   substr(Survey_Date, 3, 4)),
    ym = paste(year, month, sep = "-")
  )

nj_all_month_updated <- monthly_trimmed %>%
  mutate(
    Place_Name_Clean = case_when(
      tolower(Place_Name_Clean) %in% c("princeton township", "princeton borough") ~ "princeton",
      tolower(Place_Name_Clean) %in% c("west paterson borough") ~ "woodland park borough",
      tolower(Place_Name_Clean) %in% c("pahaquarry township") ~ "hardwick township",
      tolower(Place_Name_Clean) %in% c("dover township") & County_Code == 29 ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("south belmar borough") ~ "lake como borough",
      tolower(Place_Name_Clean) %in% c("pine valley borough") ~ "pine hill borough",
      tolower(Place_Name_Clean) %in% c("toms river town") ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("washington township") & County_Name == "Mercer" ~ "robbinsville township",
      tolower(Place_Name_Clean) %in% c("peapack and gladstone boro") ~ "peapack and gladstone borough",
      tolower(Place_Name_Clean) %in% c("orange") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("south orange village") ~ "south orange village township",
      tolower(Place_Name_Clean) %in% c("west orange town") ~ "west orange township",
      tolower(Place_Name_Clean) %in% c("orange township city") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("orange township") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("orange township city") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("pahaquarry township (n)") ~ "hardwick township",
      tolower(Place_Name_Clean) %in% c("walpack township (n)") ~ "walpack township",
      tolower(Place_Name_Clean) %in% c("carney's point township") ~ "carneys point township",
      tolower(Place_Name_Clean) %in% c("hawthorn borough") ~ "hawthorne borough",
      tolower(Place_Name_Clean) %in% c("verona borough township") ~ "verona township",
      tolower(Place_Name_Clean) %in% c("glen ridge borough township") ~ "glen ridge borough",
      tolower(Place_Name_Clean) %in% c("north caldwell township") ~ "north caldwell borough",
      tolower(Place_Name_Clean) %in% c("caldwell borough township") ~ "caldwell borough",
      tolower(Place_Name_Clean) %in% c("passaic township") ~ "long hill township",
      tolower(Place_Name_Clean) %in% c("essex fells township") ~ "essex fells borough",
      tolower(Place_Name_Clean) %in% c("westhampton township") ~ "westampton township",
      tolower(Place_Name_Clean) %in% c("belleville town") ~ "belleville township",
      tolower(Place_Name_Clean) %in% c("bloomfield town") ~ "bloomfield township",
      tolower(Place_Name_Clean) %in% c("nutley town") ~ "nutley township",
      tolower(Place_Name_Clean) %in% c("verona borough") ~ "verona township",
      tolower(Place_Name_Clean) %in% c("west caldwell borough") ~ "west caldwell township",
      tolower(Place_Name_Clean) %in% c("matawan township") ~ "aberdeen township",
      tolower(Place_Name_Clean) %in% c("irvington town") ~ "irvington township",
      tolower(Place_Name_Clean) %in% c("montclair town") ~ "montclair township",
      TRUE ~ Place_Name_Clean)
  )%>%
  group_by(Place_Name_Clean, year, ym) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
            County_Name = first(County_Name),.groups = "drop")


#### year vs. monthly comparison ----

monthly_summary <- nj_all_month_updated  %>%
  group_by(Place_Name_Clean,County_Name, Year)  %>%
  summarise(monthly_total = sum(multi_family+single_family, na.rm = TRUE),
            .groups = "drop")

nj_allx$annual_total <- rowSums(cbind(nj_allx$multi_family,nj_allx$single_family))

quality_df <- nj_allx %>%
  left_join(monthly_summary, by = c("Place_Name_Clean","County_Name", "Year"))

quality_df <- quality_df %>%
  mutate(
    quality_flag = case_when(
      is.na(monthly_total) ~ "no_monthly_data",
      abs(monthly_total - annual_total) < 5 ~ "full_match",
      monthly_total < annual_total ~ "partial_reporting",
      monthly_total > annual_total ~ "possible_error"
    )
  )

table(quality_df$quality_flag)

full_match<- quality_df %>%
  filter(quality_flag== "full_match")
  

no_monthly_data <- quality_df %>%
  filter(quality_flag== "no_monthly_data")

possible_error <- quality_df %>%
  filter(quality_flag== "possible_error")

#### original analysis updated ----

names(nj_all_updated)

annual_reporting_pct <- nj_all_updated %>%
  group_by(Place_Name_Clean) %>%
  summarise(
    County_Name = first(County_Name),
    total_months_possible = n() * 12,
    total_months_reported = sum(Months_Reported, na.rm = TRUE),
    months_reporting_pct = 100 * total_months_reported / total_months_possible,
    total_units_reported = sum(multi_family+single_family, na.rm = TRUE)
  )

reporting_summary_by_place <- nj_all_updated %>%
  left_join(annual_reporting_pct %>%
              select(Place_Name_Clean, months_reporting_pct),
            by = "Place_Name_Clean")


# reporting_summary_by_place <- monthly_reporting_summary %>%
#   group_by(Place_Name_Clean) %>%
#   summarise(
#     County_Name = first(County_Name),
#     years_total = n(),
#     years_full = sum(reporting_type == "monthly_full"),
#     years_partial = sum(reporting_type == "monthly_partial"),
#     years_sparse = sum(reporting_type == "monthly_sparse"),
#     years_none = sum(reporting_type == "no_reporting"),
#     total_units_reported = sum(total_units, na.rm = TRUE)
#   ) %>%
#   mutate(
#     consistent_reporter = years_full / years_total >= 0.75,
#     low_reporter = years_none / years_total >= 0.5
#   )%>%
#   mutate(
#     mixed_reporter = !consistent_reporter & !low_reporter,
#     reporting_score = ((1 * years_full + 0.66 * years_partial + 0.33 * years_sparse) / years_total)*100)
# 
# table(reporting_summary_by_place$reporting_score)


##### visuals updated ----



ggplot(annual_reporting_pct, aes(x = months_reporting_pct)) +
  geom_histogram(binwidth = 5, color = "black", fill = "steelblue") +
  geom_vline(xintercept = 80, linetype = "dashed", color = "red")+
  # facet_wrap(~ County_Name)+
  labs(
    title = "Distribution of Monthly Reporting Percentage",
    x = "Monthly Reporting % (All Years)",
    y = "Number of Places"
  )


##### updated annual + monthly merge ----

merged_test <- merge(cs_data,annual_reporting_pct,by=c("Place_Name_Clean","County_Name"))
merged_test$treated <- ifelse(merged_test$G > 0,"treated","control")
merged_test$post <- factor(
  ifelse(merged_test$G > merged_test$Year, "post-treatment", "pre-treatment"),
  levels = c("pre-treatment", "post-treatment")
)

merged_test <- merged_test %>% 
  mutate(reporting_category = case_when(
    consistent_reporter ~ "Consistent Reporter",
    low_reporter ~ "Low Reporter",
    TRUE ~ "Mixed Reporter"
  ))

reporting_summary_long <- merged_test %>%
  pivot_longer(
    cols = c(years_full, years_partial, years_sparse, years_none),
    names_to = "reporting_type",
    values_to = "count"
  )

ggplot(reporting_summary_long, aes(x = count, fill = treated)) +
  geom_histogram(aes(y = after_stat(density)),position = "identity", alpha = 0.5, bins = 40,binwidth = 1) +
  facet_wrap(~ reporting_type, scales = "free_y") +
  labs(
    title = "Distribution of Reporting Years by Type (Treated vs. Control)",
    x = "Number of Years",
    y = "Number of Cities"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = years_full, y = log(total_units_reported+1), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Years with Full (12-month) Reporting",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = log(population), y = log(total_units_reported), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~post)+
  labs(
    title = "log Population vs. log Total Units Reported (by Treatment)",
    x = "log Population",
    y = "log Total Housing Units Reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = (population), y = months_reporting_pct, color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~post)+
  labs(
    title = "log Population vs. log Total Units Reported (by Treatment)",
    x = "log Population",
    y = "log Total Housing Units Reported"
  ) +
  theme_minimal()

treatment_units <- merged_test %>%
  filter(G > 0) %>%
  distinct(id, population)

min(treatment_units$population, na.rm = TRUE)

# Treated do before an after treatment









ggplot(merged_test %>% filter(G>0), aes(x = years_full, y = log(total_units_reported+1), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(reporting_category~post)+
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Reporting score",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

summary(lm(log(total_units_reported+1)~treated*post,merged_test))
summary(lm(log(total_units_reported + 1) ~ factor(event_time) * reporting_category, data = merged_test_fixest))

ggplot(merged_test, aes(x = reporting_score, y = (years_full), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fully Reported Years vs. years fully reported",
    x = "Reporting score",
    y = "years fully reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = years_full, y = reporting_score, color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~post)+
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Years with Full (12-month) Reporting",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = reporting_category, fill = treated)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of Cities by Reporting Category and Treatment",
    x = "Reporting Category",
    y = "Count of Cities"
  ) +
  theme_minimal()


summary(lm(log(total_units_reported+1) ~ treated*reporting_score + factor(Year),merged_test))
summary(lm(sf_log_permits_per_1000 ~ treated*reporting_category, data = merged_test))


reporting_treatment_table <- merged_test %>%
  distinct(Place_Name_Clean, treated, reporting_category) %>%
  count(treated, reporting_category)%>%
  arrange(reporting_category)

# Print the table
print(reporting_treatment_table)

merged_test %>%
  filter(reporting_category == "Consistent Reporter") %>%
  distinct(Place_Name_Clean, treated) %>%
  arrange(treated,Place_Name_Clean)








#### Visuals ----
reporting_summary_long <- reporting_summary_by_place %>%
  pivot_longer(cols = starts_with("years_"), 
               names_to = "reporting_type", 
               values_to = "count") %>%
  filter(reporting_type %in% c("years_full", "years_partial", "years_sparse", "years_none"))

ggplot(reporting_summary_long, aes(x = count, fill = reporting_type)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  facet_wrap(~ reporting_type, scales = "free_y") +
  labs(
    title = "Distribution of Reporting Years by Type",
    x = "Number of Years",
    y = "Number of Cities"
  ) +
  theme_minimal()

ggplot(reporting_summary_by_place, aes(x = years_full, y = log(total_units_reported))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Fully Reported Years vs. Total Units Reported",
    x = "Years with Full (12-month) Reporting",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

reporting_flags <- reporting_summary_by_place %>%
  mutate(category = case_when(
    consistent_reporter ~ "Consistent Reporter",
    low_reporter ~ "Low Reporter",
    TRUE ~ "Mixed Reporter"
  ))

ggplot(reporting_flags, aes(x = category)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Number of Cities by Reporting Category",
    x = "Reporting Category",
    y = "Count of Cities"
  ) +
  theme_minimal()


#### annual + monthly merge ----

merged_test <- merge(cs_data,reporting_summary_by_place,by=c("Place_Name_Clean","County_Name"))
merged_test$treated <- ifelse(merged_test$G > 0,"treated","control")

merged_test <- merged_test %>% 
  mutate(reporting_category = case_when(
    consistent_reporter ~ "Consistent Reporter",
    low_reporter ~ "Low Reporter",
    TRUE ~ "Mixed Reporter"
  ))

reporting_summary_long <- merged_test %>%
  pivot_longer(
    cols = c(years_full, years_partial, years_sparse, years_none),
    names_to = "reporting_type",
    values_to = "count"
  )

ggplot(reporting_summary_long, aes(x = count, fill = treated)) +
  geom_histogram(aes(y = after_stat(density)),position = "identity", alpha = 0.5, bins = 40,binwidth = 1) +
  facet_wrap(~ reporting_type, scales = "free_y") +
  labs(
    title = "Distribution of Reporting Years by Type (Treated vs. Control)",
    x = "Number of Years",
    y = "Number of Cities"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = years_full, y = log(total_units_reported+1), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Years with Full (12-month) Reporting",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = log(population), y = log(total_units_reported), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "log Population vs. log Total Units Reported (by Treatment)",
    x = "log Population",
    y = "log Total Housing Units Reported"
  ) +
  theme_minimal()


# Treated do before an after treatment









ggplot(merged_test %>% filter(G>0), aes(x = years_full, y = log(total_units_reported+1), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(reporting_category~post)+
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Reporting score",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

summary(lm(log(total_units_reported+1)~treated*post,merged_test))
summary(lm(log(total_units_reported + 1) ~ factor(event_time) * reporting_category, data = merged_test_fixest))

ggplot(merged_test, aes(x = reporting_score, y = (years_full), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fully Reported Years vs. years fully reported",
    x = "Reporting score",
    y = "years fully reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = years_full, y = reporting_score, color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~post)+
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Years with Full (12-month) Reporting",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

ggplot(merged_test, aes(x = reporting_category, fill = treated)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of Cities by Reporting Category and Treatment",
    x = "Reporting Category",
    y = "Count of Cities"
  ) +
  theme_minimal()


summary(lm(log(total_units_reported+1) ~ treated*reporting_score + factor(Year),merged_test))
summary(lm(sf_log_permits_per_1000 ~ treated*reporting_category, data = merged_test))


reporting_treatment_table <- merged_test %>%
  distinct(Place_Name_Clean, treated, reporting_category) %>%
  count(treated, reporting_category)%>%
  arrange(reporting_category)

# Print the table
print(reporting_treatment_table)

merged_test %>%
  filter(reporting_category == "Consistent Reporter") %>%
  distinct(Place_Name_Clean, treated) %>%
  arrange(treated,Place_Name_Clean)






