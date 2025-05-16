options(max.print = 10000)
library(beepr)
library(tidyverse)
library(readxl)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(arrow)


### Cross walks ----
nj_permits_csv <- read.csv('/Users/matthewhockert/Downloads/NJ_Construction_Permit_Data_20250515.csv')

nj_crosswalk <- read.csv('../NJ_Municipality_Crosswalk.csv')
nj_county_city_crosswalk <- read_excel('../nj_county_city_crosswalk.xlsx')

dewey_rental_micro <- read.csv('../sample_dewey.csv')
names(dewey_rental_micro)

df <- read_parquet("../places_annual-2.parquet")

nj_permits <- read.csv(
  "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region/ne9311y.txt",
  skip = 1,
  stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)

folder_path <- "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region"

parse_survey_date <- function(x) {
  if (is.na(x)) return(NA)
  x <- as.character(x)
  n <- nchar(x)
  
  if (n == 4) {
    year <- as.integer(substr(x, 1, 2))
    month <- as.integer(substr(x, 3, 4))
    full_year <- ifelse(year >= 80, 1900 + year, 2000 + year)
    return(as.Date(sprintf("%04d-%02d-01", full_year, month)))
  } else if (n == 6) {
    return(as.Date(paste0(x, "01"), format = "%Y%m%d"))
  } else {
    return(NA)
  }
}


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
files <- list.files(folder_path, pattern = "y\\.txt$", full.names = TRUE)
nj_data_list <- lapply(files, read_permit_file)

# Get union of all column names
all_cols <- unique(unlist(lapply(nj_data_list, names)))

# Standardize columns across all files
# nj_data_list <- lapply(nj_data_list, function(df) {
#   for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
#   df <- df[all_cols]
#   return(df)
# })

nj_data_list <- lapply(nj_data_list, function(df) {
  for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
  df <- df[all_cols]
  
  if ("Zip_Code" %in% names(df)) df$Zip_Code <- as.character(df$Zip_Code)
  if ("Survey_Date" %in% names(df)) df$Survey_Date <- as.character(df$Survey_Date)
  
  return(df)
})


# Final bind and cleanup
nj_all <- bind_rows(nj_data_list)
beep()


# Optional fixes for character formatting
# nj_all2 <- nj_all %>%
#   mutate(
#     Zip_Code = as.character(Zip_Code),
#     Survey_Date = as.character(Survey_Date),
#     Date = as.Date(sapply(Survey_Date, parse_survey_date), origin = "1970-01-01"),
#     Year = format(Date, "%Y")
#   )
nj_all <- filter(nj_all, State_Code == "34")
nj_all2 <- nj_all %>%
  mutate(
    Zip_Code = as.character(Zip_Code),
    Survey_Date = as.character(Survey_Date),
    Date = as.Date(sapply(Survey_Date, parse_survey_date)),
    Date = `names<-`(Date, NULL),
    Year = as.numeric(format(Date, "%Y"))
  )
print(unique(nj_all2$Year))
beep()

nj_all2 %>%
  filter(Place_Name == "Woodland Park Borough") %>%
  pull(Year) %>%
  range()

nj_all2 <- nj_all2 %>%
  rename(
    Place_ID = X6_Digit_ID,
    Months_Reported = Number_of_Months_Rep,
    
    # Permits (non-representative sample)
    Bldgs_1U = Bldgs,
    Units_1U = X1_unit_Units,
    Value_1U = Value,
    
    Bldgs_2U = Bldgs_1,
    Units_2U = X2_units_Units,
    Value_2U = Value_1,
    
    Bldgs_3_4U = Bldgs_2,
    Units_3_4U = X3_4_units_Units,
    Value_3_4U = Value_2,
    
    Bldgs_5U = Bldgs_3,
    Units_5U = X5_units_Units,
    Value_5U = Value_3,
    
    # Representative sample
    Bldgs_1U_Rep = Bldgs_4,
    Units_1U_Rep = X1_unit_rep_Units,
    Value_1U_Rep = Value_4,
    
    Bldgs_2U_Rep = Bldgs_5,
    Units_2U_Rep = X2_units_rep_Units,
    Value_2U_Rep = Value_5,
    
    Bldgs_3_4U_Rep = Bldgs_6,
    Units_3_4U_Rep = X3_4_units_rep_Units,
    Value_3_4U_Rep = Value_6,
    
    Bldgs_5U_Rep = Bldgs_7,
    Units_5U_Rep = X5_units_rep_Units
  )

names(nj_all)

clean_place_name <- function(x) {
  x <- gsub("\\.+", "", x)           # remove all periods
  x <- gsub("#", "", x) 
  x <- trimws(x)                       # remove leading/trailing whitespace
  x <- stringr::str_to_lower(x)        # capitalize first letter of each word
  return(x)
}
nj_all2 <- nj_all2 %>%
  mutate(
    Place_Name_Clean = clean_place_name(Place_Name)
  )
# 
# nj_all2 <- nj_all2 %>%
#   mutate(
#     Place_Name_Clean = Place_Name_Clean,
#     Place_Name_Clean = gsub("\\.+", "", Place_Name_Clean),     # remove periods
#     Place_Name_Clean = gsub("#", "", Place_Name_Clean),        # remove stray hash symbols
#     Place_Name_Clean = str_trim(Place_Name_Clean),             # trim whitespace
#     Place_Name_Clean = str_to_title(Place_Name_Clean)          # capitalize each word
#   )


nj_all2 <- nj_all2 %>% filter(!is.na(Date))
nj_all2 <- nj_all2 %>%
  mutate(
    single_family = as.numeric(Units_1U),
    multi_family = rowSums(
      cbind(
        as.numeric(Units_2U),
        as.numeric(Units_3_4U),
        as.numeric(Units_5U)
      ),
      na.rm = TRUE
    )
  )

nrow(nj_all2)
nj_allx <- merge(nj_all2,nj_county_city_crosswalk,by="County_Code")
nrow(nj_allx)

nj_all_updated <- nj_allx %>%
  mutate(
    Place_Name_Clean = case_when(
      tolower(Place_Name_Clean) %in% c("princeton township", "princeton borough") ~ "princeton",
      tolower(Place_Name_Clean) %in% c("west paterson borough") ~ "woodland park borough",
      tolower(Place_Name_Clean) %in% c("pahaquarry township") ~ "hardwick township",
      tolower(Place_Name_Clean) %in% c("dover township") & County_Code == 29 ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("south belmar borough") ~ "lake como borough",
      tolower(Place_Name_Clean) %in% c("pine valley borough") ~ "pine hill borough",
      tolower(Place_Name_Clean) %in% c("toms river town") ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("washington township") & County_Code == 21 ~ "robbinsville township",
      TRUE ~ Place_Name_Clean
    )
  ) %>%
  group_by(Place_Name_Clean, Date, Year) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
            County_Name = first(County_Name),.groups = "drop")

names(nj_all_updated)
nj_all_updated <- nj_all_updated %>%
  select(1:12, CBSA_Code,County_Name, multi_family, single_family)

nj_all_updated <- nj_all_updated %>%
  filter(Year < 2025)

nj_all_updated <- nj_all_updated %>%
  group_by(Place_Name_Clean) %>%
  mutate(across(
    c(State_Code, Place_ID, County_Code, Place_Code, MSA_CMSA, PMSA_Code, Central_City),
    ~ ifelse(.x == 0 & any(.x != 0, na.rm = TRUE), max(.x[.x != 0], na.rm = TRUE), .x)
  )) %>%
  ungroup()

nrow(nj_all_updated)
nj_all_updated <- nj_all_updated %>%
  mutate(month = month(Date)) %>%
  filter(month == 12)
nrow(nj_all_updated)


nj_all_updated %>%
  group_by(Place_Name_Clean, Date) %>%
  filter(n() > 1) %>%
  summarise(dup_count = n(), .groups = "drop")

nj_all_updated %>%
  filter(Place_Name_Clean == "Newark", Year == "1998") %>%
  select(Place_Name_Clean, Date, single_family, multi_family)

nj_all_updated %>%
  filter(Place_Name_Clean == "Newark",Year == "2006") %>%
  duplicated() %>%
  sum()

nj_all_updated %>%
  filter(Place_Name_Clean == "Newark", Year == "2006") %>%
  group_by(Year) %>%
  summarise(n_obs = n(),
            sf_units = sum(single_family, na.rm = TRUE),
            mf_units = sum(multi_family, na.rm = TRUE))

#follows a cummulative pattern
nj_all_updated <- nj_all_updated %>%
  mutate(Year2 = format(Date, "%Y"))

nj_all_updated %>%
  #filter(Year >= 2010, Year <= 2015) %>%
  filter(grepl("newark", Place_Name_Clean, ignore.case = TRUE)) %>%
  filter(Place_Name_Clean != "East Newark Borough") %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(mf_units = sum(multi_family, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x= Year, y = mf_units, group = Place_Name_Clean, color = Place_Name_Clean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "",
    x = "Year",
    y = "Multifamily Units",
    color = "Place Name"
  ) +
  theme_minimal()


nj_summary <- nj_all_updated %>%
  group_by(Year, Place_Name_Clean) %>%
  summarise(multi_family = sum(multi_family, na.rm = TRUE),
            single_family = sum(single_family, na.rm = TRUE),
            .groups = "drop")

ggplot(nj_summary, aes(x = Year, y = multi_family, color = Place_Name_Clean,group = Place_Name_Clean)) +
  geom_line() +
  labs(title = "New Jersey Monthly 1-Unit Permits", x = "Date", y = "Permits Issued") +
  theme_minimal()

print(unique(rent_control_intensity$Municipality))
print(unique(nj_all_updated$Place_Name))


#### Continuous treatment ----
rent_control_intensity <- read_excel("../rent_control_scored_output.xlsx")

rent_control_intensity <- rent_control_intensity %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

rent_control_intensity <- merge(rent_control_intensity, nj_crosswalk,
                                by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE)

merged_df <- merge(nj_all_updated, rent_control_intensity,
                   by.x = "Place_Name_Clean", by.y = "nj_all_match")

"woodland park borough" %in% nj_crosswalk$rent_control_name
"woodland park borough" %in% merged_df$Place_Name_Clean
"woodland park borough" %in% rent_control_intensity$Municipality_Clean

library(lubridate)

nj_summary <- merged_df %>%
  filter(Year >= 2018, Year <= 2024)
print(unique(nj_summary$Place_Name_Clean))


bldgs_change <- nj_summary %>%
  mutate(period = ifelse(Year < 2022, "pre", "post")) %>%
  group_by(Place_Name_Clean, period) %>%
  summarise(multi_family = sum(multi_family, na.rm = TRUE),
            RCI = first(rent_control_unweighted), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = multi_family) %>%
  mutate(log_change = log(post+1) - log(pre+1))
summary(lm(log_change ~ RCI, bldgs_change))

#bldgs_change$log_change <- log(bldgs_change$Year_2024 + 1) - log(bldgs_change$Year_2018 + 1)
bldgs_change$level_change <- bldgs_change$post - bldgs_change$pre

summary(bldgs_change)
table(complete.cases(bldgs_change$log_change, bldgs_change$RCI))

plot(bldgs_change$RCI,bldgs_change$log_change)
summary(lm(log_change ~ RCI, bldgs_change))
summary(lm(level_change ~ RCI, bldgs_change))

ggplot(bldgs_change, aes(x = RCI, y = log_change)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  theme_minimal()

ggplot(bldgs_change, aes(x = RCI, y = level_change)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  theme_minimal()


bldgs_change$RCI_bin <- cut(bldgs_change$RCI, breaks = quantile(bldgs_change$RCI, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
summary(lm(log_change ~ RCI_bin, data = bldgs_change))


ggplot(merged_df, aes(x = log(multi_family), y = log(single_family), color = rent_control_unweighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c(option = "B", begin = 0.2, end = 0.9) +
  theme_minimal()

ggplot(merged_df %>% filter(Year == "2024"), aes(x = rent_control_unweighted, y = single_family)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  theme_minimal()

merged_df <- merged_df %>%
  filter(Year >=2018) %>%
  mutate(period = ifelse(Year <=2022, "pre", "post")) 
  
ggplot(merged_df, aes(x = rent_control_unweighted, y = single_family)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  facet_wrap(~period)+
  theme_minimal()
merged_df %>%
  filter(multi_family < 0) %>%
  pull(Place_Name_Clean)

plot(factor(merged_df$new_construction_exempt),merged_df$rent_control_unweighted)

#rent_control_pca
summary(lm(log(multi_family+1) ~ factor(new_construction_exempt)*rent_control_unweighted + factor(Year) + factor(Place_Name_Clean), data = merged_df))
summary(lm(log(multi_family+1) ~ factor(rent_control_unweighted) + factor(Year) + factor(Place_Name_Clean), data = merged_df))

summary(lm(log(single_family+1) ~  factor(new_construction_exempt)*rent_control_unweighted + factor(Year) + factor(Place_Name_Clean), data = merged_df))
summary(lm(log(single_family+1) ~ rent_control_unweighted + factor(Year) + factor(Place_Name_Clean), data = merged_df))
table(merged_df$new_construction_exempt)

model <- lm(
  log(multi_family + 1) ~ rent_control_unweighted * factor(new_construction_exempt) * period + 
    factor(Name) + factor(Year),
  data = merged_df
)
summary(model)
library(ggplot2)
library(dplyr)

# Create prediction grid
pred_grid <- expand.grid(
  rent_control_unweighted = seq(0, 1, length.out = 100),
  new_construction_exempt = c(0, 0.5, 1),
  period = c("pre", "post"),
  Name = "Bayonne",     # Replace with any city in your data
  Year = 2022           # Hold year constant
)

# Predict
# model_simple <- lm(
#   log(multi_family + 1) ~ rent_control_unweighted * new_construction_exempt * period,
#   data = merged_df
# )
# summary(model_simple)
pred_grid$new_construction_exempt <- factor(pred_grid$new_construction_exempt, levels = c(0, 0.5, 0.75, 1))
pred_grid$predicted <- predict(model, newdata = pred_grid)

# Plot
ggplot(pred_grid, aes(x = rent_control_unweighted, y = predicted, color = factor(new_construction_exempt))) +
  geom_line(aes(linetype = period)) +
  labs(
    title = "Predicted Multifamily Permits by RCI, Exemption, and Period",
    x = "Rent Control Intensity (RCI)",
    y = "Log(Permits + 1)",
    color = "New Const. Exempt Level",
    linetype = "Period"
  ) +
  theme_minimal()
pred_grid



#### staggered timing ----
library(did)

nj_survey_filtered <- nj_survey_dates %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality))) %>%
  filter(
    ordinance_date_1 > ymd("2000-01-01"),
    latest_pre2022_ordinance_date > ymd("2000-01-01"),
    latest_pre2022_ordinance_date < ymd("2020-01-01")
  )

nj_survey_filtered <- merge(
  nj_survey_filtered,
  nj_crosswalk,
  by.x = "Municipality_Clean",
  by.y = "rent_control_name",
  all.x = TRUE
)

nj_survey_filtered$treatment_year <- format(nj_survey_filtered$ordinance_date_1, "%Y")
nj_survey_filtered$repealed_year <- format(as.Date(nj_survey_filtered$repealed_date), "%Y")

staggered_rc_permits <- merge(
  nj_all_updated,
  nj_survey_filtered,
  by.x = "Place_Name_Clean",
  by.y = "nj_all_match",
  all.x = TRUE
)



intensity_rc_panel <- merge(
  nj_all_updated,
  rent_control_intensity,
  by.x = "Place_Name_Clean",
  by.y = "nj_all_match",
  all.x = TRUE
)

#staggered_rc_permits$Year
staggered_rc_permits$treatment_year = ifelse(is.na(staggered_rc_permits$treatment_year), 0, staggered_rc_permits$treatment_year)
table(staggered_rc_permits$treatment_year)
staggered_rc_permits_ag <- staggered_rc_permits %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(
    multi_family_sum = sum(multi_family, na.rm = TRUE),
    multi_family_mean = mean(multi_family, na.rm = TRUE),
    treatment_year = as.numeric(first(treatment_year)),
    repealed_year = first(repealed_year),
    county = first(County_Code),
    msa = first(MSA._CMSA),
    central_city = as.factor(first(Central_City)),
    .groups = "drop"
  ) %>%
  mutate(
    treatment_year = ifelse(is.na(treatment_year), 0, treatment_year),
    treated = ifelse(treatment_year > 0,T,F),
    year_num = as.numeric(Year),
    id = as.numeric(factor(Place_Name_Clean))
  )
table(staggered_rc_permits_ag$treatment_year)

staggered_rc_permits_ag <- staggered_rc_permits_ag %>%
  filter(!is.na(treatment_year))

staggered_rc_permits_ag <- staggered_rc_permits_ag %>%
  group_by(Place_Name_Clean, Year) %>%
  mutate(log_mf_mean = log(multi_family_mean + 1),
         log_mf_sum = log(multi_family_sum + 1)) %>%
  ungroup()

invalid_munis <- nj_survey_dates %>%
  filter(is.na(ordinance_date_1) | latest_ordinance_date < ymd("2000-01-01")) %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality))) %>%
  pull(Municipality_Clean)
invalid_munis
# union city
staggered_rc_permits_ag <- staggered_rc_permits_ag %>%
  filter(!Place_Name_Clean %in% invalid_munis)%>%
  arrange(Place_Name_Clean,Year)

table(staggered_rc_permits_ag$treatment_year)
nrow(staggered_rc_permits_ag)
staggered_rc_permits_ag <- merge(staggered_rc_permits_ag, nj_muni_df,by.x = "Place_Name_Clean", by.y="Municipality_Clean")
nrow(staggered_rc_permits_ag)

ggplot(staggered_rc_permits_ag %>% filter(Place_Name_Clean=="belleville township"), 
       aes(x = year_num, y = multi_family_sum, color = treated)) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Pre-Treatment Trends", y = "Log Multi-Family Permits")

hist(log(staggered_rc_permits_ag$multi_family_sum/staggered_rc_permits_ag$SQ_MILES+1))
staggered_rc_permits_ag$multi_family_sq_mile <- log(staggered_rc_permits_ag$multi_family_sum/staggered_rc_permits_ag$SQ_MILES+1)

staggered_rc_permits_ag %>%
  mutate(treated_this_year = year_num >= treatment_year & treatment_year > 0) %>%
  group_by(Year) %>%
  summarise(
    n_true = sum(treated_this_year, na.rm = TRUE),
    total = n(),
    share_true = 100 * n_true / total
  ) %>%
  print(n = 100)

ggplot(staggered_rc_permits_ag, 
       aes(x = year_num, y = multi_family_sq_mile, color = treated)) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Pre-Treatment Trends", y = "Log Multi-Family Permits")

att_gt_out <- att_gt(
  yname = "multi_family_mean",
  tname = "year_num",
  idname = "id",
  gname = "treatment_year",
  data = staggered_rc_permits_ag,
  panel = T,
  xformla = ~MUN_TYPE + central_city,
  control_group = "nevertreated",
  est_method = "reg"
)

dynamic_att <- aggte(att_gt_out, type = "dynamic",na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)

staggered_rc_permits_ag %>%
  group_by(treatment_year) %>%
  summarise(
    treated_n = sum(year == treatment_year, na.rm = TRUE),
    total_n = n_distinct(id),
    .groups = "drop"
  ) %>%
  arrange(treatment_year)

staggered_rc_permits_ag %>%
  filter(treatment_year > 0) %>%
  distinct(id, treatment_year) %>%
  count(treatment_year, name = "n_treated") %>%
  arrange(treatment_year)


table(staggered_rc_permits_ag$treatment_year, useNA = "ifany")


 ##### V2 ----

library(lubridate)

rent_dates <- readxl::read_excel("../rent_control_raw_dates_by_city2.xlsx")

rent_dates2 <- rent_dates %>%
  mutate(
    Municipality_Clean = str_to_lower(str_trim(Municipality)),
    RentControl_Parsed = map(RentControl, function(x) {
      if (is.na(x) || str_trim(x) == "") return(NA_Date_)
      raw_parts <- str_split(x, ";")[[1]]
      parsed_dates <- map_chr(raw_parts, function(date_str) {
        date_str <- str_trim(date_str)
        if (str_detect(date_str, "^\\d{4}$")) paste0(date_str, "-01-01") else date_str
      })
      dates <- suppressWarnings(ymd(parsed_dates))
      dates[!is.na(dates)]
    }),
    first_treatment_year = map_int(RentControl_Parsed, ~ if (all(is.na(.))) NA_integer_ else year(min(., na.rm = TRUE))),
    first_amendment_year = map_int(RentControl_Parsed, function(d) {
      if (length(d) < 2) NA_integer_
      else year(sort(d)[2])
    }),
    last_treatment_year = map_int(RentControl_Parsed, ~ if (all(is.na(.))) NA_integer_ else year(max(., na.rm = TRUE))),
    first_post_2000_treatment_year = map_int(RentControl_Parsed, function(d) {
      post2000 <- d[d > ymd("2000-01-01")]
      if (length(post2000) == 0) NA_integer_ else year(min(post2000))
    }),
    treatment_count = map_int(RentControl_Parsed, ~ length(unique(year(.))))
  ) %>%
  select(-RentControl_Parsed)
# rent_dates_clean <- rent_dates2 %>%
#   mutate(RentControl = str_replace_all(RentControl, "\\s+", "")) %>%
#   separate_rows(RentControl, sep = ";") %>%
#   filter(!is.na(RentControl), RentControl != "") %>%
#   mutate(
#     RentControl = case_when(
#       RentControl == "28058" ~ NA_character_,
#       RentControl == "August12,2013" ~ "2013-08-12",
#       RentControl == "3-8-2021" ~ "2021-03-08",
#       str_detect(RentControl, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ as.character(mdy(RentControl)),
#       str_detect(RentControl, "^\\d{4}-\\d{2}-\\d{2}$") ~ RentControl,
#       str_detect(RentControl, "^\\d{4}$") ~ paste0(RentControl, "-01-01"),
#       TRUE ~ suppressWarnings(as.character(mdy(RentControl)))
#     )
#   )

rent_dates2 <- merge(rent_dates2, nj_crosswalk,
                    by.x = "Municipality_Clean",
                    by.y = "rent_control_name", all.x = TRUE)

# rent_dates2$treatment_year <- year(as.numeric(rent_dates2$RentControl))


staggered_rc_permits <- merge(
  nj_all_updated,
  rent_dates2,
  by.x = "Place_Name_Clean",
  by.y = "nj_all_match",
  all.x = TRUE
)
# intensity_rc_panel <- merge(
#   nj_all_updated,
#   rent_control_intensity,
#   by.x = "Place_Name_Clean",
#   by.y = "nj_all_match",
#   all.x = TRUE
# )

staggered_rc_permits$treatment_year = ifelse(is.na(staggered_rc_permits$treatment_year), 0, staggered_rc_permits$treatment_year)
table(staggered_rc_permits$treatment_year)
staggered_rc_permits_ag <- staggered_rc_permits %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(
    single_family_sum = sum(single_family, na.rm = TRUE),
    single_family_mean = mean(single_family, na.rm = TRUE),
    multi_family_sum = sum(multi_family, na.rm = TRUE),
    multi_family_mean = mean(multi_family, na.rm = TRUE),
    treatment_year = as.numeric(first(treatment_year)),
    first_treatment_year = as.numeric(first(first_treatment_year)),
    last_treatment_year = as.numeric(first(last_treatment_year)),
    first_post_2000_treatment_year = as.numeric(first(first_post_2000_treatment_year)),
    treatment_count = as.numeric(first(treatment_count)),
    county = first(County_Code),
    msa = first(MSA._CMSA),
    central_city = as.factor(first(Central_City)),
    .groups = "drop"
  ) %>%
  mutate(
    treatment_year = ifelse(is.na(treatment_year), 0, treatment_year),
    first_treatment_year = ifelse(is.na(first_treatment_year), 0, first_treatment_year),
    first_post_2000_treatment_year = ifelse(is.na(first_post_2000_treatment_year), 0, first_post_2000_treatment_year),
    last_treatment_year = ifelse(is.na(last_treatment_year), 0, last_treatment_year),
    treatment_count = ifelse(is.na(treatment_count), 0, treatment_count),
    treated = ifelse(treatment_year > 0,T,F),
    year_num = as.numeric(Year),
    id = as.numeric(factor(Place_Name_Clean))
  )
table(staggered_rc_permits_ag$treatment_year)

staggered_rc_permits_ag <- staggered_rc_permits_ag %>%
  filter(!is.na(treatment_year))

staggered_rc_permits_ag <- staggered_rc_permits_ag %>%
  group_by(Place_Name_Clean, Year) %>%
  mutate(log_mf_mean = log(multi_family_mean + 1),
         log_mf_sum = log(multi_family_sum + 1),
         log_sf_mean = log(single_family_mean + 1),
         log_sf_sum = log(single_family_sum + 1)) %>%
  ungroup()

invalid_munis <- nj %>%
  filter(is.na(RentControl)) %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality))) %>%
  pull(Municipality_Clean)
invalid_munis
# union city
staggered_rc_permits_ag <- staggered_rc_permits_ag %>%
  filter(!Place_Name_Clean %in% invalid_munis)%>%
  arrange(Place_Name_Clean,Year)

table(staggered_rc_permits_ag$treatment_year)
nrow(staggered_rc_permits_ag)
staggered_rc_permits_ag <- merge(staggered_rc_permits_ag, nj_muni_df,by.x = "Place_Name_Clean", by.y="Municipality_Clean")
nrow(staggered_rc_permits_ag)

ggplot(staggered_rc_permits_ag %>% filter(Place_Name_Clean=="belleville township"), 
       aes(x = year_num, y = multi_family_sum, color = treated)) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Pre-Treatment Trends", y = "Log Multi-Family Permits")


hist(log(staggered_rc_permits_ag$multi_family_sum/staggered_rc_permits_ag$SQ_MILES+1))
staggered_rc_permits_ag$multi_family_sq_mile <- log(staggered_rc_permits_ag$multi_family_sum/staggered_rc_permits_ag$SQ_MILES+1)

staggered_rc_permits_ag %>%
  mutate(treated_this_year = year_num >= treatment_year & treatment_year > 0) %>%
  group_by(Year) %>%
  summarise(
    n_true = sum(treated_this_year, na.rm = TRUE),
    total = n(),
    share_true = 100 * n_true / total
  ) %>%
  print(n = 100)

ggplot(staggered_rc_permits_ag %>% filter(first_treatment_year < 2000| first_treatment_year == 0), 
       aes(x = year_num, y = log_sf_sum, color = treated)) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Pre-Treatment Trends", y = "Log Multi-Family Permits")

ggplot(staggered_rc_permits_ag, 
       aes(x = year_num, y = multi_family_sq_mile, color = treated)) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Multi-Family Permits per Square Mile", y = "Log MF Permits/Sq Mile")

ggplot(staggered_rc_permits_ag %>% 
         filter(first_post_2000_treatment_year > year_num),
       aes(x = year_num, y = log_mf_sum, color = as.factor(first_post_2000_treatment_year))) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Pre-Treatment MF Trends by Treatment Year", color = "First Treatment Year")

names(staggered_rc_permits_ag)
att_gt_out <- att_gt(
  yname = "multi_family_sq_mile",
  tname = "year_num",
  idname = "id",
  gname = "first_post_2000_treatment_year",
  data = staggered_rc_permits_ag,
  panel = F,
  xformla = ~MUN_TYPE.x,
  control_group = "notyettreated",
  est_method = "dr"
)

dynamic_att <- aggte(att_gt_out, type = "dynamic",na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)
beep()


#### continous/staggered ----


# Merge rent_control_intensity and nj_survey_filtered into a single dataset
# Data gets weird here
rc_combined <- merge(
  rent_control_intensity, rent_dates2,
  by = "Municipality_Clean", suffixes = c("_rc", "_dates")
)

nrow(rc_combined)

plot(log(rc_combined$treatment_count),rc_combined$rent_control_unweighted)
summary(lm(rent_control_unweighted ~ log(treatment_count)+factor(year),rc_combined))


##### Exploring timing by city ----
sampled_munis <- rc_combined %>%
  filter(!is.na(RentControl) & first_post_2000_treatment_year > 2000) %>%
  slice_sample(n = 6) %>%
  pull(Municipality_Clean)

# Filter panel data to those municipalities
explore_df <- staggered_rc_permits_ag %>%
  filter(Place_Name_Clean %in% sampled_munis)

# Match rent control dates to each municipality
rentcontrol_dates <- rc_combined %>%
  select(Municipality_Clean, RentControl) %>%
  mutate(
    RentControl_Parsed = map(RentControl, function(x) {
      if (is.na(x) || str_trim(x) == "") return(NA_Date_)
      parsed <- suppressWarnings(ymd(str_split(x, ";")[[1]]))
      parsed[!is.na(parsed) & parsed > ymd("2000-01-01")]
    })
  )

plots <- map(sampled_munis, function(muni) {
  muni_data <- explore_df %>% filter(Place_Name_Clean == muni)
  dates <- rentcontrol_dates %>%
    filter(Municipality_Clean == muni) %>%
    pull(RentControl_Parsed) %>%
    .[[1]]
  
  base_plot <- ggplot(muni_data, aes(x = year_num, y = multi_family_sum)) +
    geom_line(color = "steelblue") +
    geom_point() +
    labs(
      title = str_to_title(muni),
      x = "Year", y = "Multi-Family Permits"
    ) +
    theme_minimal()
  
  # Add one geom_vline per date (if any)
  if (!is.null(dates) && length(dates) > 0) {
    base_plot <- reduce(
      dates,
      .init = base_plot,
      .f = function(p, d) p + geom_vline(xintercept = year(d), linetype = "dashed", color = "red")
    )
  }
  
  base_plot
})
print(p)
# Print them one by one
for (p in plots) print(p)

# Join to nj_all only once
con_stag <- merge(
  nj_all_updated,
  rc_combined,
  by.x = "Place_Name_Clean", by.y = "nj_all_match_rc", all.x = TRUE
)

colSums(is.na(nj_all_updated))
colSums(is.na(con_stag))

rc_events <- con_stag %>%
  select(Place_Name_Clean, RentControl) %>%
  filter(!is.na(RentControl)) %>%
  mutate(RentControlDate = str_split(RentControl, ";")) %>%
  unnest(RentControlDate) %>%
  mutate(RentControlDate = ymd(RentControlDate)) %>%
  filter(RentControlDate >= ymd("2000-01-01"))%>%
  filter(!is.na(RentControlDate)) %>%
  mutate(RentControlYear = year(RentControlDate)) %>%
  distinct(Place_Name_Clean, RentControlYear)

# Step 2: Join to con_stag and compute event time in years
con_stag_events <- con_stag %>%
  inner_join(rc_events, by = "Place_Name_Clean", relationship = "many-to-many") %>%
  mutate(
    Year = as.numeric(Year),
    RentControlYear = as.numeric(RentControlYear),
    years_since_rc = Year - RentControlYear
  ) %>%
  filter(years_since_rc >= -20, years_since_rc <= 20)

# Step 3: Aggregate mean log(multi_family) by event year
agg_effects <- con_stag_events %>%
  filter(!is.na(multi_family)) %>%
  group_by(years_since_rc) %>%
  summarise(mean_log_mf = mean(log1p(multi_family), na.rm = TRUE)) %>%
  arrange(years_since_rc)
print(agg_effects,n=100)
# Step 4: Plot
ggplot(agg_effects, aes(x = years_since_rc, y = mean_log_mf)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Average Multi-Family Permits by Years Since Rent Control",
    x = "Years Since Rent Control Implementation",
    y = "Mean Log(Multi-Family Permits)"
  )
ggplot(agg_effects, aes(x = years_since_rc, y = mean_log_mf)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Avg Log(MF Permits) by Years Since Rent Control Adoption",
       x = "Years Since Rent Control", y = "Mean log1p(MF Permits)") +
  theme_minimal()

summary(fixest::feols(log1p(multi_family) ~ i(years_since_rc, ref = -1)*factor(rent_control_unweighted) | Place_Name_Clean + Year, data = con_stag_events))

con_stag_events <- con_stag_events %>%
  mutate(rc_intensity_group = ifelse(rent_control_unweighted >= .5, "High", "Low"))

# Run separate models
feols(log1p(multi_family) ~ i(years_since_rc, ref = -1) | Place_Name_Clean + Year,
      data = filter(con_stag_events, rc_intensity_group == "High")) %>%
  summary()

feols(log1p(multi_family) ~ i(years_since_rc, ref = -1) | Place_Name_Clean + Year,
      data = filter(con_stag_events, rc_intensity_group == "Low")) %>%
  summary()

con_stag_events <- con_stag_events %>%
  mutate(rc_intensity_high = rent_control_unweighted >= median(rent_control_unweighted, na.rm = TRUE))

feols(log1p(multi_family) ~ i(years_since_rc, rc_intensity_group, ref = -1) | Place_Name_Clean + Year,
               data = con_stag_events)%>%
  summary()

# Aggregate mean permits by event time and intensity group
agg_plot_df <- con_stag_events %>%
  filter(!is.na(multi_family)) %>%
  group_by(years_since_rc, rc_intensity_group) %>%
  summarise(mean_log_mf = mean(log1p(multi_family), na.rm = TRUE), .groups = "drop")
print(agg_plot_df,n=100)
# Plot
ggplot(agg_plot_df, aes(x = years_since_rc, y = mean_log_mf, color = rc_intensity_group)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Mean Log(Multi-Family Permits) Around Rent Control Adoption",
    x = "Years Since Rent Control",
    y = "Mean log(1 + Multi-Family Permits)",
    color = "RC Intensity"
  ) +
  theme_minimal()

con_stag %>%
  filter(!is.na(rent_control_unweighted), !is.na(first_treatment_year)) %>%
  mutate(pre2000 = first_treatment_year < 2000) %>%
  group_by(pre2000) %>%
  summarise(
    mean_rci = mean(rent_control_unweighted, na.rm = TRUE),
    median_rci = median(rent_control_unweighted, na.rm = TRUE),
    n = n()
  )
ggplot(con_stag %>% filter(!is.na(rent_control_unweighted), !is.na(first_treatment_year)), 
       aes(x = factor(first_treatment_year < 2000), y = rent_control_unweighted)) +
  geom_boxplot() +
  labs(x = "Adopted Before 2000", y = "Rent Control Intensity",
       title = "RCI Comparison: Pre- vs Post-2000 Adopters")


# Step 1

amendment_counts_normalized <- con_stag %>%
  select(Place_Name_Clean, RentControl) %>%
  filter(!is.na(RentControl)) %>%
  distinct() %>%
  mutate(
    dates_list = map(RentControl, ~ suppressWarnings(ymd(str_split(.x, ";")[[1]]))),
    dates_list = map(dates_list, ~ .x[!is.na(.x)]),
    first_date = map_dbl(dates_list, ~ if (length(.x) == 0) NA_real_ else min(.x, na.rm = TRUE) %>% as.numeric()),
    amendment_count = map2_int(dates_list, first_date, ~ sum(.x > as.Date(.y, origin = "1970-01-01"))),
    first_date_actual = as.Date(first_date, origin = "1970-01-01"),
    years_active = 2022 - year(first_date_actual),
    amendment_count_per_year = ifelse(years_active > 0, amendment_count / years_active, NA_real_)
  ) %>%
  select(Place_Name_Clean, amendment_count, years_active, amendment_count_per_year)

print(as_tibble(amendment_counts_normalized), n = 100)

rent_control_summary <- con_stag %>%
  filter(!is.na(first_treatment_year), !is.na(rent_control_unweighted)) %>%
  mutate(pre2000 = first_treatment_year < 2000) %>%
  distinct(Place_Name_Clean, pre2000,rent_control_unweighted, cap_numeric, cpi_tied, units_covered, sf_exempt,
           new_construction_exempt, hardship_appeals)

amendment_summary <- rent_control_summary %>%
  left_join(amendment_counts_normalized, by = "Place_Name_Clean") %>%
  group_by(pre2000) %>%
  summarise(
    mean_amendments = mean(amendment_count, na.rm = TRUE),
    median_amendments = median(amendment_count, na.rm = TRUE),
    n = n(),
    mean_amendments_norm = mean(amendment_count_per_year, na.rm = TRUE)
  )

print(amendment_summary)
# Summarize by pre-/post-
rent_control_summary %>%
  group_by(pre2000) %>%
  summarise(across(c(rent_control_unweighted,cap_numeric, cpi_tied, units_covered, sf_exempt,
                     new_construction_exempt, hardship_appeals), ~mean(.x, na.rm = TRUE)))

amendment_rci_df <- amendment_counts %>%
  left_join(rent_control_summary %>% select(Place_Name_Clean, rent_control_unweighted), by = c("Place_Name_Clean"))

summary(lm(rent_control_unweighted ~ amendment_count, data = amendment_rci_df))

top_amenders <- amendment_counts %>%
  arrange(desc(amendment_count)) %>%
  slice_head(n = 10)

con_stag %>%
  filter(Place_Name_Clean %in% top_amenders$Place_Name_Clean) %>%
  pivot_longer(cols = c(RentIncreaseLimit, Exemptions, UnitsStructure, VacancyIncrease, VacancyControl, 
                        HardshipIncreases, CapitalImprovements, AdministrativeProcedures, EvictionControls,
                        RegistrationRequirements, FeeSchedules, ExpirationOrSunset, Definitions),
               names_to = "Component", values_to = "Dates") %>%
  filter(!is.na(Dates)) %>%
  count(Place_Name_Clean, Component, sort = TRUE) %>%
  group_by(Place_Name_Clean) %>%
  top_n(3, wt = n) %>%
  arrange(Place_Name_Clean)%>%
  print(n=100)

# hollowed_repealed_flags <- con_stag %>%
#   distinct(Place_Name_Clean, repealed_year, units_covered, cap_numeric, rent_control_unweighted) %>%
#   mutate(
#     repealed = !is.na(repealed_year),
#     hollowed_out = units_covered < 0.25 | cap_numeric > 10 | rent_control_unweighted < 0.2
#   )
# 
# table(hollowed_repealed_flags$repealed, hollowed_repealed_flags$hollowed_out)

date_cols <- c(
  "RentControl", "RentIncreaseLimit", "Exemptions", "UnitsStructure",
  "VacancyIncrease", "VacancyControl", "HardshipIncreases", "CapitalImprovements",
  "AdministrativeProcedures", "EvictionControls", "RegistrationRequirements",
  "FeeSchedules", "ExpirationOrSunset", "Definitions"
)

timeline_data <- con_stag %>%
  select(Municipality_Clean, all_of(date_cols)) %>%
  pivot_longer(cols = -Municipality_Clean, names_to = "Component", values_to = "Dates") %>%
  filter(!is.na(Dates)) %>%
  mutate(Date = str_split(Dates, ";")) %>%
  unnest(Date) %>%
  mutate(Date = ymd(str_trim(Date))) %>%
  filter(!is.na(Date))

selected_cities <- c("newark city", "jersey city", "hoboken city", "asbury park city", "montclair township")

timeline_data %>%
  filter(Municipality_Clean %in% selected_cities) %>%
  ggplot(aes(x = Date, y = fct_rev(Component), color = Component)) +
  geom_point() +
  facet_wrap(~ str_to_title(Municipality_Clean), scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Rent Control Policy Timelines by City",
    x = "Date of Ordinance",
    y = "Policy Component"
  ) +
  theme(legend.position = "none")


pre2000_rc_events <- con_stag %>%
  filter(!is.na(RentControl)) %>%
  distinct(Place_Name_Clean, RentControl) %>%
  filter(Place_Name_Clean %in% rent_control_summary$Place_Name_Clean[rent_control_summary$pre2000]) %>%
  mutate(
    amendment_dates = map(RentControl, ~ suppressWarnings(ymd(str_split(.x, ";")[[1]]))),
    amendment_dates = map(amendment_dates, ~ .x[!is.na(.x) & .x >= ymd("2000-01-01")])
  ) %>%
  unnest(amendment_dates) %>%
  distinct(Place_Name_Clean, amendment_date = amendment_dates)

amendment_event_panel <- con_stag %>%
  filter(Place_Name_Clean %in% pre2000_rc_events$Place_Name_Clean) %>%
  inner_join(pre2000_rc_events, by = "Place_Name_Clean",relationship = "many-to-many") %>%
  mutate(years_since_amendment = as.numeric(Year) - amendment_year) %>%
  filter(years_since_amendment >= -5, years_since_amendment <= 10)

library(fixest)
summary(
  feols(
    log1p(multi_family) ~ i(years_since_amendment, ref = -1) | Place_Name_Clean + Year,
    data = amendment_event_panel
  )
)%>%summary()




names(treated_df)
nrow(treated_df)
treated_df2 <- merge(treated_df, nj_muni_df,by.x = "Place_Name_Clean", by.y="Municipality_Clean")
nrow(treated_df2)
treated_df2 <- treated_df2 %>%
  mutate(
    real_treatment_year = case_when(
      !is.na(treatment_year) & `Units-in-Structure Ordinance Applies to` != "Mobile Homes only" ~ treatment_year,
      TRUE ~ NA_real_
    )
  )

treated_df2 %>%
  filter(!is.na(rent_control_unweighted), is.na(real_treatment_year)) %>%
  distinct(Place_Name_Clean, Year, real_treatment_year, rent_control_unweighted)%>%
  arrange(Place_Name_Clean, Year)

treated_df2 <- treated_df2 %>%
  filter(!(is.na(real_treatment_year) & !is.na(rent_control_unweighted)))
treated_df2$multi_family_sq_mile <- treated_df2$multi_family/treated_df2$SQ_MILES
treated_df2 <- filter(treated_df2,Year < "2025")
bldgs_change <- treated_df2 %>%
  # filter(Year >= treatment_year - 4 & Year <= treatment_year + 4) %>%
  mutate(period = ifelse(Year < real_treatment_year, "pre", "post")) %>%
  group_by(Place_Name_Clean, period) %>%
  summarise(
    multi_family = mean(multi_family, na.rm = TRUE),
    RCI = first(rent_control_unweighted),
    real_treatment_year = first(real_treatment_year),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = period, values_from = multi_family) %>%
  mutate(log_change = log(post + 1) - log(pre + 1))

summary(lm(log_change ~ RCI, data = bldgs_change))
summary(lm(log_change ~ RCI + factor(real_treatment_year), data = bldgs_change))

ggplot(bldgs_change, aes(x = RCI, y = log_change)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  theme_minimal()

bldgs_change %>%
  mutate(RCI_bin = cut(RCI, breaks = quantile(RCI, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)) %>%
  ggplot(aes(x = RCI_bin, y = log_change)) +
  geom_boxplot() +
  labs(x = "RCI Quartile", y = "Log Change in Multifamily Permits") +
  theme_minimal()

treated_df2 %>%
  mutate(RCI_q = ntile(rent_control_unweighted, 4),
         period = ifelse(Year < real_treatment_year, "Pre", "Post")) %>%
  group_by(RCI_q, period) %>%
  summarise(sum_mf = sum(multi_family, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = period, y = sum_mf, fill = factor(RCI_q))) +
  geom_col(position = "dodge") +
  labs(x = "Period", y = "Avg. Multifamily Permits", fill = "RCI Quartile") +
  theme_minimal()


ggplot(treated_df2, aes(x = log(multi_family), y = log(single_family), color = rent_control_unweighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c(option = "B", begin = 0.2, end = 0.9) +
  theme_minimal()

ggplot(treated_df2, aes(x = rent_control_unweighted, y = single_family)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  theme_minimal()
treated_df2 <- treated_df2 %>%
  mutate(treated = !is.na(real_treatment_year))

treated_df2 %>%
  filter(!is.na(real_treatment_year)) %>%
  mutate(
    Year = as.numeric(Year),
    real_treatment_year = as.numeric(real_treatment_year),
    rel_year = Year - real_treatment_year
  ) %>%
  filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year,treated) %>%
  summarise(mean_mf = sum(log(single_family+1), na.rm = TRUE)) %>%
  ggplot(aes(x = rel_year, y = mean_mf,group = factor(treated))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Rent Control Adoption", y = "Avg. Multifamily Permits") +
  theme_minimal()

treated_df2 %>%
  filter(!is.na(real_treatment_year)) %>%
  mutate(
    Year = as.numeric(Year),
    real_treatment_year = as.numeric(real_treatment_year),
    rel_year = Year - real_treatment_year
  ) %>%
  filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year) %>%
  summarise(
    mean_mf = mean(log(multi_family + 1), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = rel_year, y = mean_mf)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Years Since Rent Control Adoption",
    y = "Avg. Log Multifamily Permits",
    color = "Treated"
  ) +
  theme_minimal()

print(treated_df2 %>%
  filter(!is.na(real_treatment_year)) %>%
  mutate(
    Year = as.numeric(Year),
    treatment_year = as.numeric(treatment_year),
    rel_year = Year - treatment_year
  ) %>%
  filter(rel_year %in% c(2, 3, 4)) %>%
  group_by(Place_Name_Clean,rel_year) %>%
  summarise(total_mf = sum(log(multi_family+1), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_mf)),n=200)

library(tidyr)

treated_df2 %>%
  filter(!is.na(real_treatment_year)) %>%
  mutate(
    Year = as.numeric(Year),
    treatment_year = as.numeric(treatment_year),
    rel_year = Year - treatment_year
  ) %>%
  filter(rel_year %in% c(2, 3, 4)) %>%
  group_by(Place_Name_Clean, rel_year) %>%
  summarise(total_mf = sum(log(multi_family+1), na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = rel_year,
    values_from = total_mf,
    names_prefix = "year_"
  ) %>%
  arrange(desc(year_3), desc(year_2), desc(year_4))

treated_df2 %>%
  # filter(tolower(Place_Name_Clean) %in% c("wall township", "hamilton township")) %>%
  filter(`Units-in-Structure Ordinance Applies to` == "Mobile Homes only")%>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(mf_units = sum(multi_family, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = as.numeric(Year), y = mf_units, color = Place_Name_Clean)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Multifamily Permits Over Time",
    x = "Year",
    y = "Multifamily Units",
    color = "Municipality"
  ) +
  theme_minimal()
  
treated_df2 %>%
  filter(treated == TRUE) %>%
  distinct(Place_Name_Clean) %>%
  nrow()
table(treated_df2$Year,treated_df2$treatment_year)

treated_df2 %>%
  filter(!is.na(treatment_year)) %>%
  mutate(
    Year = as.integer(Year),
    real_treatment_year = as.integer(treatment_year),
    rel_year = Year - real_treatment_year
  ) %>%
  # filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year) %>%
  summarise(mean_mf = mean(log(multi_family+1), na.rm = TRUE)) %>%
  ggplot(aes(x = rel_year, y = mean_mf)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Rent Control Adoption", y = "Avg. Multifamily Permits") +
  theme_minimal()

treated_df2 %>%
  filter(!is.na(real_treatment_year), !is.na(rent_control_unweighted)) %>%
  # group_by(real_treatment_year)%>%
  mutate(
    Year = as.integer(Year),
    real_treatment_year = as.integer(real_treatment_year),
    rel_year = Year - real_treatment_year,
    RCI_bin = ntile(rent_control_unweighted, 4),
    RCI = factor(rent_control_unweighted)
  ) %>%
  ungroup()%>%
  # filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year, RCI_bin) %>%
  summarise(mean_mf = mean(log(multi_family+1), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = rel_year, y = (mean_mf), color = factor(RCI_bin))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    color = "RCI Quartile",
    x = "Years Since Rent Control Adoption",
    y = "Avg. Multifamily Permits"
  ) +
  theme_minimal()


library(dplyr)
library(ggplot2)

# Step 1: Fix bin assignment once per municipality
rci_bins_fixed <- treated_df2 %>%
  filter(!is.na(real_treatment_year), !is.na(rent_control_unweighted)) %>%
  mutate(real_treatment_year = as.integer(real_treatment_year)) %>%
  filter(Year == real_treatment_year) %>%
  group_by(Place_Name_Clean, real_treatment_year) %>%
  summarise(
    RCI_at_treat = first(rent_control_unweighted),
    .groups = "drop"
  ) %>%
  mutate(RCI_bin = ntile(RCI_at_treat, 4))

# Step 2: Merge on BOTH Place_Name_Clean AND real_treatment_year
treated_df2_binned <- treated_df2 %>%
  mutate(
    Year = as.integer(Year),
    real_treatment_year = as.integer(real_treatment_year)
  ) %>%
  left_join(rci_bins_fixed, by = c("Place_Name_Clean", "real_treatment_year")) %>%
  mutate(rel_year = Year - real_treatment_year)

# Step 3: Plot dynamic effects by RCI_bin
treated_df2_binned %>%
  filter(!is.na(rel_year), !is.na(RCI_bin)) %>%
  group_by(rel_year, RCI_bin) %>%
  summarise(mean_mf = mean(log(multi_family + 1), na.rm = TRUE),
            mean_mf = mean(log(multi_family_sq_mile + 1), na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = rel_year, y = mean_mf, color = factor(RCI_bin))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    color = "RCI Quartile",
    x = "Years Since Rent Control Adoption",
    y = "Avg. Log(Multifamily Permits + 1)"
  ) +
  theme_minimal()



print(treated_df2_binned %>%
        filter(!is.na(rel_year), !is.na(RCI_bin)) %>%
        group_by(rel_year, RCI_bin) %>%
        summarise(mean_mf = mean(log(multi_family + 1), na.rm = TRUE), .groups = "drop"),n=1000)

print(unique(treated_df2$real_treatment_year))
es_df <- treated_df2 %>%
  #filter(!is.na(real_treatment_year)) %>%
  mutate(
    Year = as.integer(Year),
    real_treatment_year = as.integer(real_treatment_year),
    rel_year = Year - real_treatment_year,
    RCI_bin = ntile(rent_control_unweighted, 4)) 
#%>% filter(rel_year >= -5, rel_year <= 5)


summary(lm(log(multi_family+1) ~ factor(RCI_bin)*factor(rel_year) + Place_Name_Clean + factor(Year), data = es_df))

subset_12 <- es_df %>% filter(RCI_bin %in% c(1, 2))
summary(lm(log(multi_family + 1) ~ factor(RCI_bin) * factor(rel_year) + Place_Name_Clean + factor(Year), data = subset_12))

subset_34 <- es_df %>% filter(RCI_bin %in% c(3, 4))

summary(lm(log(multi_family + 1) ~ factor(RCI_bin) * factor(rel_year) + Place_Name_Clean + factor(Year), data = subset_34))



treated_df2 %>%
  filter(!is.na(real_treatment_year)) %>%
  mutate(
    rel_year = as.integer(Year) - as.integer(real_treatment_year)
  ) %>%
  filter(rel_year >= -5, rel_year <= 5) %>%
  ggplot(aes(x = rel_year, y = multi_family, color = rent_control_unweighted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_viridis_c() +
  theme_minimal()

feols(log(multi_family + 1) ~ rel_year * rent_control_unweighted | Place_Name_Clean + Year, data = es_df)

es_df$log_multi_family <- log(es_df$multi_family+1)
es_df$id <- as.numeric(factor(es_df$Place_Name_Clean))
es_df$real_treatment_year[is.na(es_df$real_treatment_year)] <- 0
es_df <- es_df %>%
  mutate(
    id = as.numeric(id),
    time = as.numeric(Year),
    G = as.numeric(real_treatment_year),  # G is the first treatment year
    treated = !is.na(G)
  )
print(unique(es_df$G))

print(es_df %>%
  group_by(Place_Name_Clean, G) %>%
  summarise(
    n_years = n_distinct(time),
    min_year = min(time, na.rm = TRUE),
    max_year = max(time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  count(G, n_years) %>%
  arrange(G, desc(n_years)),n=200)

str(es_df_balanced %>%
      select(id, time, G, log_multi_family))

s_df_yearly <- es_df %>%
  group_by(id, time, G) %>%
  summarise(
    multi_family = sum(multi_family, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(log_multi_family = log(multi_family + 1))
table(s_df_yearly$time,s_df_yearly$G)
# Run on all groups 
cs_mod <- att_gt(
  yname = "log_multi_family",        # or log(multi_family + 1)
  tname = "time",
  idname = "id",
  gname = "G",
  data = s_df_yearly %>%
    select(id, time, G, log_multi_family),
  panel = T,
  control_group = "nevertreated"
)

dynamic_att <- aggte(cs_mod, type = "dynamic",na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)
beep()

print(unique(es_df$G))

# Visualize the never treated group
never_treated_binned <- es_df %>%
  filter(G == 0) %>%
  mutate(
    Year = as.integer(Year),
    RCI_bin = 0
  ) %>%
  select(Place_Name_Clean, Year, multi_family, RCI_bin)


# Step 2: Select needed cols from treated_df2_binned
rci_bins_fixed <- treated_df2 %>%
  filter(!is.na(real_treatment_year), !is.na(rent_control_unweighted)) %>%
  filter(Year == real_treatment_year) %>%
  group_by(Place_Name_Clean) %>%
  summarise(RCI_at_treat = first(rent_control_unweighted), .groups = "drop") %>%
  mutate(RCI_bin = ntile(RCI_at_treat, 4))

treated_binned_trimmed <- treated_df2_binned %>%
  filter(!is.na(RCI_bin)) %>%
  mutate(Year = as.integer(Year)) %>%
  select(Place_Name_Clean, Year, multi_family, RCI_bin)

# treated_df2_binned <- treated_df2 %>%
#   left_join(rci_bins_fixed, by = "Place_Name_Clean")

# Step 3: Combine and summarize
combined_by_year <- bind_rows(treated_binned_trimmed, never_treated_binned) %>%
  group_by(Year, RCI_bin) %>%
  summarise(mean_mf = mean(log(multi_family + 1), na.rm = TRUE), .groups = "drop")

# Step 4: Plot
ggplot(combined_by_year, aes(x = Year, y = mean_mf, color = factor(RCI_bin))) +
  geom_line() +
  geom_point() +
  labs(
    color = "RCI Quartile (0 = Never Treated)",
    x = "Calendar Year",
    y = "Avg. Log(Multifamily Permits + 1)"
  ) +
  theme_minimal()
print(combined_by_year,n=200)

es_df_yearly_bin <- es_df %>%
  group_by(id, time, G,RCI_bin) %>%
  summarise(
    mean_multi_family_sq_mile = mean(multi_family_sq_mile,na.rm = T),
    multi_family = sum(multi_family, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(log_multi_family = log(multi_family))

table(s_df_yearly$time,s_df_yearly$G)

es_rci1 <- es_df_yearly_bin %>% filter(RCI_bin == 1 | G==0)
print(unique(es_rci1$G))

cs_mod_rci1 <- att_gt(
  yname = "multi_family_sq_mile",           # or log(multi_family + 1)
  tname = "time",
  idname = "id",
  gname = "G",
  data = es_rci1,
  panel = T,
  control_group = "notyettreated"
)

dynamic_att <- aggte(cs_mod_rci1, type = "dynamic",na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)
beep()
