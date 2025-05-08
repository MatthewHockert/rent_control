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
  header1 <- readLines(path, n = 1)
  header2 <- readLines(path, n = 2)[2]
  
  split1 <- strsplit(header1, ",")[[1]]
  split2 <- strsplit(header2, ",")[[1]]
  
  combined_names <- ifelse(split1 == "", split2, paste0(split1, "_", split2))
  combined_names <- make.names(combined_names, unique = TRUE)
  
  df <- read.csv(path, skip = 2, header = FALSE, stringsAsFactors = FALSE)
  names(df) <- combined_names
  return(df)
}
files <- list.files(folder_path, pattern = "y\\.txt$", full.names = TRUE)
nj_data_list <- lapply(files, read_permit_file)

all_cols <- unique(unlist(lapply(nj_data_list, names)))

nj_data_list <- lapply(nj_data_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  for (col in missing_cols) df[[col]] <- NA
  df <- df[match(all_cols, names(df))]  # enforce column order
  df
})

nj_data_list <- lapply(nj_data_list, function(df) {
  if ("Zip_Code" %in% names(df)) df$Zip_Code <- as.character(df$Zip_Code)
  if ("Survey_Date" %in% names(df)) df$Survey_Date <- as.character(df$Survey_Date)
  df
})

nj_all <- bind_rows(nj_data_list)
nj_all <- filter(nj_all, State_Code == "34")

nj_all$Date <- as.Date(paste0(nj_all$Survey_Date, "01"), format = "%Y%m%d")
nj_all$Year <- format(nj_all$Date, "%Y")

nj_all <- nj_all %>%
  mutate(Name = gsub("\\.+", "", Place_Name),
         Name = gsub("#", "", Place_Name),
         Name = str_trim(Place_Name),
         Name = str_to_title(Place_Name))
nj_all <- nj_all %>% filter(!is.na(Date))
nj_all$multi_family <- nj_all$Bldgs.1+nj_all$Bldgs.2+nj_all$Bldgs.3+nj_all$Bldgs.4+nj_all$Bldgs.5
nj_all$single_family <- nj_all$Bldgs

nj_summary <- nj_all %>%
  mutate(Bldgs = as.numeric(Bldgs)) %>%
  group_by(Year, Name) %>%
  summarise(Bldgs = sum(Bldgs, na.rm = TRUE),
            multi_family = sum(multi_family, na.rm = TRUE),
            single_family = sum(single_family, na.rm = TRUE),
            .groups = "drop")

ggplot(nj_summary, aes(x = Year, y = multi_family, color = Name,group = Name)) +
  geom_line() +
  labs(title = "New Jersey Monthly 1-Unit Permits", x = "Date", y = "Permits Issued") +
  theme_minimal()

print(unique(rent_control_intensity$Municipality))
print(unique(nj_all$Place_Name))


#### Continuous treatment ----
rent_control_intensity <- read_excel("../rent_control_scored_output.xlsx")
nj_all <- nj_all %>%
  mutate(Place_Name_Clean = str_to_lower(str_trim(Place_Name)))

rent_control_intensity <- rent_control_intensity %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

rent_control_intensity <- merge(rent_control_intensity, nj_crosswalk,
                                by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE)

merged_df <- merge(nj_all, rent_control_intensity,
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
nj_survey_filtered <- merge(nj_survey_filtered, nj_crosswalk,
                                by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE)
nj_survey_filtered$treatment_year <- format(nj_survey_filtered$earliest_ordinance_date, "%Y")
nj_survey_filtered$repealed_year <- format(as.Date(nj_survey_filtered$repealed_date), "%Y")

staggered_rc_permits <- merge(nj_all, nj_survey_filtered,
                   by.x = "Place_Name_Clean", by.y = "nj_all_match", all.x = TRUE)


staggered_rc_permits$Year.x

staggered_rc_permits_ag <- staggered_rc_permits %>%
  group_by(Name, Year.x) %>%
  summarise(single_family = mean(Bldgs, na.rm = TRUE),
            multi_family = mean(multi_family,na.rm = T),
            log_multi_family = log(multi_family+1),
            treatment_year = first(treatment_year),
            repealed_year = first(repealed_year),
            .groups = "drop")

staggered_rc_permits_ag$treatment_year[is.na(staggered_rc_permits_ag$treatment_year)] <- 0
staggered_rc_permits_ag$year_num <- as.numeric(staggered_rc_permits_ag$Year.x)
staggered_rc_permits_ag$treatment_year <- as.numeric(staggered_rc_permits_ag$treatment_year)

staggered_rc_permits_ag$repealed_year[is.na(staggered_rc_permits_ag$repealed_year)] <- 0
staggered_rc_permits_ag$year_num <- as.numeric(staggered_rc_permits_ag$Year.x)
staggered_rc_permits_ag$repealed_year <- as.numeric(staggered_rc_permits_ag$repealed_year)

#staggered_rc_permits_ag$Name <- as.numeric(staggered_rc_permits_ag$Name)
staggered_rc_permits_ag$id <- as.numeric(factor(staggered_rc_permits_ag$Name))

print(unique(staggered_rc_permits_ag$treatment_year))
print(unique(staggered_rc_permits_ag$repealed_year))

table(staggered_rc_permits_ag$treatment_year)
table(staggered_rc_permits_ag$repealed_year)


att_gt_out <- att_gt(
  yname = "log_multi_family",
  tname = "year_num",
  idname = "id",
  gname = "treatment_year",
  data = staggered_rc_permits_ag,
  panel = T,
  # xformla = ~ Name,
  control_group = "nevertreated",
  est_method = "reg"
  
)
dynamic_att <- aggte(att_gt_out, type = "dynamic")
summary(dynamic_att)
ggdid(dynamic_att)


#### continous/staggered ----
rent_control_intensity <- read_excel("../rent_control_scored_output.xlsx")

# Clean municipality names
rent_control_intensity <- rent_control_intensity %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

nj_survey_filtered <- nj_survey_filtered %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

# Merge both datasets with crosswalk to get nj_all_match
rent_control_intensity <- merge(
  rent_control_intensity, nj_crosswalk,
  by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE
)

nj_survey_filtered <- merge(
  nj_survey_filtered, nj_crosswalk,
  by.x = "Municipality_Clean", by.y = "rent_control_name", all.x = TRUE
)

# Merge rent_control_intensity and nj_survey_filtered into a single dataset
rc_combined <- merge(
  rent_control_intensity, nj_survey_filtered,
  by = "Municipality_Clean", suffixes = c("_rc", "_dates"), all = TRUE
)

# Join to nj_all only once
con_stag <- merge(
  nj_all,
  rc_combined,
  by.x = "Place_Name_Clean", by.y = "nj_all_match", all.x = TRUE
)

# Convert treatment year to numeric
con_stag$treatment_year <- as.numeric(format(con_stag$latest_ordinance_date, "%Y"))
treated_df <- con_stag %>%
  # filter(!is.na(latest_ordinance_date)) %>%
  mutate(treatment_year = year(latest_ordinance_date))

bldgs_change <- treated_df %>%
  # filter(Year >= treatment_year - 4 & Year <= treatment_year + 4) %>%
  mutate(period = ifelse(Year < treatment_year, "pre", "post")) %>%
  group_by(Name, period) %>%
  summarise(
    multi_family = mean(multi_family, na.rm = TRUE),
    RCI = first(rent_control_unweighted),
    treatment_year = first(treatment_year),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = period, values_from = multi_family) %>%
  mutate(log_change = log(post + 1) - log(pre + 1))

summary(lm(log_change ~ RCI, data = bldgs_change))
summary(lm(log_change ~ RCI + factor(treatment_year), data = bldgs_change))

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

treated_df %>%
  mutate(RCI_q = ntile(rent_control_unweighted, 4),
         period = ifelse(Year < treatment_year, "Pre", "Post")) %>%
  group_by(RCI_q, period) %>%
  summarise(mean_mf = mean(multi_family, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = period, y = mean_mf, fill = factor(RCI_q))) +
  geom_col(position = "dodge") +
  labs(x = "Period", y = "Avg. Multifamily Permits", fill = "RCI Quartile") +
  theme_minimal()


ggplot(treated_df, aes(x = log(multi_family), y = log(single_family), color = rent_control_unweighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c(option = "B", begin = 0.2, end = 0.9) +
  theme_minimal()

ggplot(treated_df, aes(x = rent_control_unweighted, y = single_family)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)+
  theme_minimal()


treated_df %>%
  filter(!is.na(treatment_year)) %>%
  mutate(rel_year = Year - as.numeric(treatment_year)) %>%
  filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year) %>%
  summarise(mean_mf = mean(multi_family, na.rm = TRUE)) %>%
  ggplot(aes(x = rel_year, y = mean_mf)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Rent Control Adoption", y = "Avg. Multifamily Permits") +
  theme_minimal()

treated_df %>%
  filter(!is.na(treatment_year)) %>%
  mutate(
    Year = as.integer(Year),
    treatment_year = as.integer(treatment_year),
    rel_year = Year - treatment_year
  ) %>%
  # filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year) %>%
  summarise(mean_mf = mean(multi_family, na.rm = TRUE)) %>%
  ggplot(aes(x = rel_year, y = mean_mf)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Years Since Rent Control Adoption", y = "Avg. Multifamily Permits") +
  theme_minimal()

treated_df %>%
  filter(!is.na(treatment_year), !is.na(rent_control_unweighted)) %>%
  mutate(
    Year = as.integer(Year),
    treatment_year = as.integer(treatment_year),
    rel_year = Year - treatment_year,
    RCI_bin = ntile(rent_control_unweighted, 4)
  ) %>%
  # filter(rel_year >= -5 & rel_year <= 5) %>%
  group_by(rel_year, RCI_bin) %>%
  summarise(mean_mf = mean(log(multi_family+1), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = rel_year, y = (mean_mf), color = as.factor(RCI_bin))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    color = "RCI Quartile",
    x = "Years Since Rent Control Adoption",
    y = "Avg. Multifamily Permits"
  ) +
  theme_minimal()

print(unique(treated_df$treatment_year))
es_df <- treated_df %>%
  #filter(!is.na(treatment_year)) %>%
  mutate(
    Year = as.integer(Year),
    treatment_year = as.integer(treatment_year),
    rel_year = Year - treatment_year,
    RCI_bin = ntile(rent_control_unweighted, 4)) 
#%>% filter(rel_year >= -5, rel_year <= 5)


summary(lm(log(multi_family+1) ~ factor(RCI_bin)*factor(rel_year) + Place_Name_Clean + factor(Year), data = es_df))

subset_12 <- es_df %>% filter(RCI_bin %in% c(1, 2))
summary(lm(log(multi_family + 1) ~ factor(RCI_bin) * factor(rel_year) + Place_Name_Clean + factor(Year), data = subset_12))

subset_34 <- es_df %>% filter(RCI_bin %in% c(3, 4))

summary(lm(log(multi_family + 1) ~ factor(RCI_bin) * factor(rel_year) + Place_Name_Clean + factor(Year), data = subset_34))



treated_df %>%
  filter(!is.na(treatment_year)) %>%
  mutate(
    rel_year = as.integer(Year) - as.integer(treatment_year)
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
es_df$treatment_year[is.na(es_df$treatment_year)] <- 0
es_df <- es_df %>%
  mutate(
    id = id,
    time = as.integer(Year),
    G = as.integer(treatment_year),  # G is the first treatment year
    treated = !is.na(G)
  )
print(unique(es_df$G))

cs_mod <- att_gt(
  yname = "multi_family",        # or log(multi_family + 1)
  tname = "time",
  idname = "id",
  gname = "G",
  data = es_df,
  panel = F,
  control_group = "notyettreated"
)

dynamic_att <- aggte(cs_mod, type = "dynamic",na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)
beep()


es_rci1 <- es_df %>% filter(RCI_bin == 4 | G==0)
print(unique(es_rci1$G))

cs_mod_rci1 <- att_gt(
  yname = "log_multi_family",           # or log(multi_family + 1)
  tname = "time",
  idname = "id",
  gname = "G",
  data = es_rci1,
  panel = FALSE,
  control_group = "nevertreated"
)

dynamic_att <- aggte(cs_mod_rci1, type = "dynamic",na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)
beep()
