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
  if (grepl("^\\d{4}$", x)) {
    prefix <- substr(x, 1, 2)
    if (as.numeric(prefix) >= 80 & as.numeric(prefix) <= 99) {
      return(as.numeric(paste0("19", prefix)))
    } else {
      return(as.numeric(x))
    }
  } else {
    return(NA)
  }
}

nj_all_month2 <- nj_all_month %>%
  mutate(
    Year = sapply(Survey_Date, parse_survey_date)
  )
print(unique(nj_all_month2$Year))
beep()

print(unique(nj_all2$Place_Name))



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

nj_all_month <- nj_all_month %>%
  mutate(
    Place_Name_Clean = clean_place_name(Place_Name)
  )

nrow(nj_all_month)
nj_allx_month <- merge(nj_all_month,nj_county_city_crosswalk,by="County_Code")
nrow(nj_allx_month)


monthly_trimmed <- nj_allx_month %>%
  select(Survey_Date, State_Code, Place_Name_Clean, County_Code, County_Name,
         X1_unit_Units, X2_units_Units, X3_4_units_Units, X5_units_Units)

monthly_trimmed <- monthly_trimmed %>%
  rename(
    sf_units = X1_unit_Units,
    two_units = X2_units_Units,
    three_four_units = X3_4_units_Units,
    mf_units = X5_units_Units
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



monthly_reporting_summary <- nj_all_month_updated %>%
  group_by(Place_Name_Clean, year) %>%
  summarise(
    County_Name = first(County_Name),
    months_reported = sum(sf_units > 0 | two_units > 0 | three_four_units > 0 | mf_units > 0),
    total_units = sum(sf_units + two_units + three_four_units + mf_units),
    .groups = "drop"
  )

monthly_reporting_summary <- monthly_reporting_summary %>%
  mutate(
    reporting_type = case_when(
      months_reported == 12 ~ "monthly_full",
      months_reported >= 6  ~ "monthly_partial",
      months_reported < 6 & months_reported > 0   ~ "monthly_sparse",
      months_reported == 0  ~ "no_reporting"
    )
  )
months_reported %>%
  group_by(Place_)
length(unique(monthly_reporting_summary$months_reported))

reporting_summary_by_place <- monthly_reporting_summary %>%
  group_by(Place_Name_Clean) %>%
  summarise(
    County_Name = first(County_Name),
    years_total = n(),
    years_full = sum(reporting_type == "monthly_full"),
    years_partial = sum(reporting_type == "monthly_partial"),
    years_sparse = sum(reporting_type == "monthly_sparse"),
    years_none = sum(reporting_type == "no_reporting"),
    total_units_reported = sum(total_units, na.rm = TRUE)
  ) %>%
  mutate(
    consistent_reporter = years_full / years_total >= 0.75,
    low_reporter = years_none / years_total >= 0.5
  )%>%
  mutate(
    mixed_reporter = !consistent_reporter & !low_reporter,
    reporting_score = ((1 * years_full + 0.66 * years_partial + 0.33 * years_sparse) / years_total)*100)

table(reporting_summary_by_place$reporting_score)

##### Visuals ----
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


# Treated do before an after treatment









ggplot(merged_test, aes(x = reporting_score, y = log(total_units_reported+1), color = treated)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fully Reported Years vs. Total Units Reported (by Treatment)",
    x = "Reporting score",
    y = "Total Housing Units Reported"
  ) +
  theme_minimal()

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
  facet_wrap(~reporting_category)+
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






