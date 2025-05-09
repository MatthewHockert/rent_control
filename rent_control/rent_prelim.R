options(max.print = 10000)
library(beepr)
library(tidyverse)
nj_crosswalk <- read.csv('../NJ_Municipality_Crosswalk.csv')

dewey_rental_micro <- read.csv('../sample_dewey.csv')
names(dewey_rental_micro)

nj_permits <- read.csv(
  "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region/ne9311y.txt",
  skip = 1,
  stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)

folder_path <- "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region"

read_permit_file <- function(path) {
  lines <- readLines(path, n = 2)
  header1 <- strsplit(lines[1], ",")[[1]]
  header2 <- strsplit(lines[2], ",")[[1]]
  
  # Combine headers: use header2 where header1 is blank
  merged_headers <- ifelse(header1 == "", header2, paste0(header1, "_", header2))
  merged_headers <- gsub("\\s+", "_", merged_headers) # replace whitespace
  merged_headers <- gsub("\\.+", ".", merged_headers) # collapse repeated dots
  merged_headers <- make.names(merged_headers, unique = TRUE)
  
  df <- read.csv(path, skip = 2, header = FALSE, stringsAsFactors = FALSE)
  
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
nj_data_list <- lapply(nj_data_list, function(df) {
  for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
  df <- df[all_cols]
  return(df)
})

nj_data_list <- lapply(nj_data_list, function(df) {
  for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
  df <- df[all_cols]
  
  if ("Zip_Code" %in% names(df)) df$Zip_Code <- as.character(df$Zip_Code)
  if ("Survey_Date" %in% names(df)) df$Survey_Date <- as.character(df$Survey_Date)
  
  return(df)
})


# Final bind and cleanup
nj_all <- bind_rows(nj_data_list)

# Optional fixes for character formatting
nj_all <- nj_all %>%
  mutate(
    Zip_Code = as.character(Zip_Code),
    Survey_Date = as.character(Survey_Date),
    Date = as.Date(paste0(Survey_Date, "01"), format = "%Y%m%d"),
    Year = format(Date, "%Y")
  )

nj_all <- filter(nj_all, State_Code == "34")
nj_all <- nj_all %>%
  rename(
    Place_ID = X6.Digit_ID,
    Months_Reported = Number_of_Months_Rep,
    
    # Permits (non-representative sample)
    Bldgs_1U = Bldgs,
    Units_1U = X1.unit_Units,
    Value_1U = Value,
    
    Bldgs_2U = Bldgs.1,
    Units_2U = X2.units_Units,
    Value_2U = Value.1,
    
    Bldgs_3_4U = Bldgs.2,
    Units_3_4U = X3.4_units_Units,
    Value_3_4U = Value.2,
    
    Bldgs_5U = Bldgs.3,
    Units_5U = X5._units_Units,
    Value_5U = Value.3,
    
    # Representative sample
    Bldgs_1U_Rep = Bldgs.4,
    Units_1U_Rep = X1.unit_rep_Units,
    Value_1U_Rep = Value.4,
    
    Bldgs_2U_Rep = Bldgs.5,
    Units_2U_Rep = X2.units_rep_Units,
    Value_2U_Rep = Value.5,
    
    Bldgs_3_4U_Rep = Bldgs.6,
    Units_3_4U_Rep = X3.4_units_rep_Units,
    Value_3_4U_Rep = Value.6,
    
    Bldgs_5U_Rep = Bldgs.7,
    Units_5U_Rep = X5._units_rep_Units
  )

names(nj_all)

nj_all$Date <- as.Date(paste0(nj_all$Survey_Date, "01"), format = "%Y%m%d")
nj_all$Year <- format(nj_all$Date, "%Y")

nj_all <- nj_all %>%
  mutate(Name = gsub("\\.+", "", Place_Name),
         Name = gsub("#", "", Place_Name),
         Name = str_trim(Place_Name),
         Name = str_to_title(Place_Name))

nj_all <- nj_all %>%
  mutate(Place_Name_Clean = str_to_lower(str_trim(Place_Name)))

nj_all <- nj_all %>% filter(!is.na(Date))
nj_all <- nj_all %>%
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

nj_all_updated <- nj_all %>%
  mutate(
    Place_Name_Clean = case_when(
      tolower(Place_Name_Clean) %in% c("princeton township", "princeton borough") ~ "princeton",
      tolower(Place_Name_Clean) %in% c("west paterson borough") ~ "woodland park borough",
      tolower(Place_Name_Clean) %in% c("pahaquarry township") ~ "hardwick township",
      tolower(Place_Name_Clean) %in% c("dover township") ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("south belmar borough") ~ "lake como borough",
      tolower(Place_Name_Clean) %in% c("pine valley borough") ~ "pine hill borough",
      tolower(Place_Name_Clean) %in% c("toms river town") ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("washington township") & County_Code == 21 ~ "robbinsville township",
      TRUE ~ Place_Name_Clean
    )
  ) %>%
  group_by(Place_Name_Clean, Date, Year) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

names(nj_all_updated)
nj_all_updated <- nj_all_updated %>%
  select(1:12, CBSA_Code, multi_family, single_family)

nj_all_updated <- nj_all_updated %>%
  filter(Year < 2025)

nj_all_updated %>%
  #filter(Year >= 2010, Year <= 2015) %>%
  filter(grepl("woodland park borough", Place_Name_Clean, ignore.case = TRUE)) %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(mf_units = sum(multi_family, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x= Year, y = mf_units, group = Place_Name_Clean, color = Place_Name_Clean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Multifamily Units in Princeton-Area Places (2010–2015)",
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
  group_by(Name, period) %>%
  summarise(multi_family = mean(multi_family, na.rm = TRUE),
            RCI = first(rent_control_unweighted), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = multi_family) %>%
  mutate(log_change = log(post + 1) - log(pre + 1))
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
  pull(Name)

plot(factor(merged_df$new_construction_exempt),merged_df$rent_control_unweighted)

#rent_control_pca
summary(lm(log(multi_family+1) ~ factor(new_construction_exempt)*rent_control_unweighted + factor(Year) + factor(Name), data = merged_df))
summary(lm(log(multi_family+1) ~ factor(rent_control_unweighted) + factor(Year) + factor(Name), data = merged_df))

summary(lm(log(single_family+1) ~  factor(new_construction_exempt)*rent_control_unweighted + factor(Year) + factor(Name), data = merged_df))
summary(lm(log(single_family+1) ~ rent_control_unweighted + factor(Year) + factor(Name), data = merged_df))


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
rent_control_intensity <- read_excel("../rent_control_scored_output.xlsx")

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


rent_control_intensity <- rent_control_intensity %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

rent_control_intensity <- merge(
  rent_control_intensity,
  nj_crosswalk,
  by.x = "Municipality_Clean",
  by.y = "rent_control_name",
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
  filter(!Place_Name_Clean %in% invalid_munis)

table(staggered_rc_permits_ag$treatment_year)
nrow(staggered_rc_permits_ag)
staggered_rc_permits_ag <- merge(staggered_rc_permits_ag, nj_muni_df,by.x = "Place_Name_Clean", by.y="Municipality_Clean")
nrow(staggered_rc_permits_ag)

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
  yname = "multi_family_sq_mile",
  tname = "year_num",
  idname = "id",
  gname = "treatment_year",
  data = staggered_rc_permits_ag,
  panel = F,
  #xformla = ~MUN_TYPE + central_city,
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


#### continous/staggered ----
rent_control_intensity <- read_excel("../rent_control_scored_output.xlsx")

# Clean municipality names - no year
rent_control_intensity <- rent_control_intensity %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))
nrow(rent_control_intensity)

nj_survey_filtered <- nj_survey_dates %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

nrow(nj_survey_filtered)
# Filter rows where the earliest ordinance date is after 2000-01-01
nj_survey_filtered <- nj_survey_filtered %>%
  group_by(Municipality_Clean)%>%
  filter(ordinance_date_1 > ymd("2000-01-01"),latest_pre2022_ordinance_date > ymd("2000-01-01") & latest_pre2022_ordinance_date < ymd("2020-01-01"))%>%
  ungroup()

nrow(nj_survey_filtered)
# Merge both datasets with crosswalk to get nj_all_match
rent_control_intensity <- merge(
  rent_control_intensity, nj_crosswalk,
  by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE
)
nrow(rent_control_intensity)

nj_survey_filtered <- merge(
  nj_survey_filtered, nj_crosswalk,
  by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE
)
nrow(nj_survey_filtered)

# Merge rent_control_intensity and nj_survey_filtered into a single dataset
# Data gets weird here
rc_combined <- merge(
  rent_control_intensity, nj_survey_filtered,
  by = "Municipality_Clean", suffixes = c("_rc", "_dates")
)

nrow(rc_combined)


nj_all_updated %>%
  group_by(Place_Name_Clean) %>%
  summarise(
    n_years = n_distinct(Year),
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  count(n_years) %>%
  arrange(desc(n_years))

# Join to nj_all only once
con_stag <- merge(
  nj_all_updated,
  rc_combined,
  by.x = "Place_Name_Clean", by.y = "nj_all_match_rc", all.x = TRUE
)

con_stag %>%
  group_by(Place_Name_Clean) %>%
  summarise(
    n_years = n_distinct(Year),
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  count(n_years) %>%
  arrange(desc(n_years))

# Convert treatment year to numeric
con_stag$treatment_year <- as.numeric(format(con_stag$latest_pre2022_ordinance_date, "%Y"))
treated_df <- con_stag %>%
  # filter(!is.na(latest_ordinance_date)) %>%
  mutate(treatment_year = year(latest_pre2022_ordinance_date))

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
