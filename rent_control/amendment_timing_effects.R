library(did)

# 1.	 Amendments that impact permitting
nrow(nj_all_updated)
nrow(con_stag)
con_stag <- merge(
  nj_all_updated,
  rc_combined,
  by.x = "Place_Name_Clean", by.y = "nj_all_match_rc", all.x = TRUE
)
nrow(con_stag)

print(con_stag%>% subset(Place_Name_Clean == "woodland park borough"))

nrow(con_stag)

# --- Step 1: Aggregate con_stag to yearly ---
con_stag_year <- con_stag %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(
    single_family_sum = sum(single_family, na.rm = TRUE),
    single_family_mean = mean(single_family, na.rm = TRUE),
    multi_family_sum = sum(multi_family, na.rm = TRUE),
    multi_family_mean = mean(multi_family, na.rm = TRUE),
    rent_control_unweighted = first(rent_control_unweighted),
    cap_numeric = first(cap_numeric),
    cpi_tied = first(cpi_tied),
    units_covered = first(units_covered),
    sf_exempt = first(sf_exempt),
    new_construction_exempt = first(new_construction_exempt),
    hardship_appeals = first(hardship_appeals),
    RentControl = first(RentControl),
    RentIncreaseLimit = first(RentIncreaseLimit),
    Exemptions = first(Exemptions),
    UnitsStructure = first(UnitsStructure),
    VacancyIncrease = first(VacancyIncrease),
    VacancyControl = first(VacancyControl),
    HardshipIncreases = first(HardshipIncreases),
    CapitalImprovements = first(CapitalImprovements),
    AdministrativeProcedures = first(AdministrativeProcedures),
    EvictionControls = first(EvictionControls),
    RegistrationRequirements = first(RegistrationRequirements),
    FeeSchedules = first(FeeSchedules),
    ExpirationOrSunset = first(ExpirationOrSunset),
    Definitions = first(Definitions),
    first_treatment_year = first(first_treatment_year),
    first_amendment_year = first(first_amendment_year),
    County_Name = first(County_Name),
    .groups = "drop"
  ) %>%
  group_by(Place_Name_Clean, Year) %>%
  mutate(log_mf_mean = log(multi_family_mean + 1),
         log_mf_sum = log(multi_family_sum + 1),
         log_sf_mean = log(single_family_mean + 1),
         log_sf_sum = log(single_family_sum + 1)) %>%
  ungroup()
nrow(con_stag_year)
# print(unique(con_stag_year$Place_Name_Clean))
# print(unique(city_population_by_year$Place_Name_Clean))
names(which(table(con_stag_year$Place_Name_Clean) != 37))
#colSums(is.na(con_stag_year))


con_stag_year %>%
  #filter(Year >= 2010, Year <= 2015) %>%
  filter(grepl("newark", Place_Name_Clean, ignore.case = TRUE)) %>%
  filter(Place_Name_Clean != "East Newark Borough") %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(mf_units = sum(multi_family_sum, na.rm = TRUE), .groups = "drop") %>%
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

nrow(con_stag_year)
con_stag_year_pop <- con_stag_year
#con_stag_year_pop <- merge(con_stag_year,nj_pop_all3,by=c("Place_Name_Clean","County_Name","Year"))
nrow(con_stag_year_pop)
names(which(table(con_stag_year_pop$Place_Name_Clean) != 37))


con_stag_year_pop <- con_stag_year_pop %>%
  group_by(Place_Name_Clean, Year) %>%
  mutate(
    mf_permits_per_1000 = (multi_family_sum / population) * 1000,
    mf_log_permits_per_1000 = log(mf_permits_per_1000 + 1),
    sf_permits_per_1000 = (single_family_sum / population) * 1000,
    sf_log_permits_per_1000 = log(sf_permits_per_1000 + 1)
  )

con_stag_year_pop <- con_stag_year_pop %>%
  mutate(
    adoption_group = case_when(
      first_treatment_year < 2000 ~ "Pre-2000",
      first_treatment_year >= 2000 ~ "Post-2000",
      is.na(first_treatment_year) ~ "nevertreated"
    )
  )

hist(con_stag_year_pop$mf_permits_per_1000)
hist(con_stag_year_pop$mf_log_permits_per_1000)

anti_join(
  con_stag_year_pop,
  nj_muni_df,
  by = c("Place_Name_Clean" = "Municipality_Clean", "County_Name" = "County_Name")
)

nrow(con_stag_year_pop)
con_stag_year2 <- merge(con_stag_year_pop, nj_muni_df_improved,by.x = c("Place_Name_Clean",'County_Name',"Year"), by.y=c("name_con",'County_Name',"Year"))
nrow(con_stag_year2)
names(which(table(con_stag_year2$Place_Name_Clean) != 37))

missing_nj_muni_con_stag <- anti_join(
  con_stag_year_pop,
  nj_muni_df_improved,
  by = c("Place_Name_Clean" = "name_con", "County_Name" = "County_Name","Year"="Year")
)


# con_stag_year2 %>%
#   filter(adoption_group == "Post-2000") %>%
#   distinct(Place_Name_Clean, first_treatment_year) %>%
#   count(first_treatment_year) 

ggplot(con_stag_year2, aes(x = Year, y = log_mf_sum,group=adoption_group, color = adoption_group)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Single Family Development Trends",
    subtitle = "Comparison of pre-2000, post-2000, and never-treated",
    x = "Year",
    y = "mf_log_permits_per_1000",
    color = "adoption_group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

con_stag_long <- con_stag_year2 %>%
  pivot_longer(
    cols = c(log_mf_sum, log_sf_sum),
    names_to = "unit_type",
    values_to = "permits"
  )

ggplot(con_stag_long, aes(x = Year, y = permits, color = unit_type, group = unit_type)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Single-Family vs Multi-Family Development Trends",
    subtitle = "Mean annual permits by type",
    x = "Year",
    y = "Permits",
    color = "Unit Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggplot(con_stag_year2 %>% filter(Place_Name_Clean %in% c("newark", "jersey city","hoboken","cape may point borough")), aes(x = Year, y = log_mf_sum, color = Place_Name_Clean, group = Place_Name_Clean)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Multi-Family Permitting Trends",
    subtitle = "Mean annual permits by city per 1000 people",
    x = "Year",
    y = "Permits",
    color = "Unit Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

zero_mf <- con_stag_year2 %>%
  group_by(Place_Name_Clean) %>%
  summarize(
    total_years = n(),
    zero_years = sum(log_mf_sum == 0, na.rm = TRUE)
  ) %>%
  filter(zero_years == total_years)

zero_permits <-con_stag_year2 %>%
  group_by(Place_Name_Clean) %>%
  summarize(
    total_years = n(),
    zero_mf = sum(log_mf_sum == 0, na.rm = TRUE),
    zero_sf = sum(log_sf_sum == 0, na.rm = TRUE)
  ) %>%
  filter(zero_mf == total_years & zero_sf == total_years)


con_stag_year2%>%
  group_by(MUN_TYPE)%>%
  summarise(mean_miles = mean(SQ_MILES,na.rm=T))

city_size_bin_lookup <- con_stag_year2 %>%
  group_by(Place_Name_Clean) %>%
  summarize(SQ_MILES = first(SQ_MILES), .groups = "drop") %>%
  mutate(
    size_bin = ntile(SQ_MILES, 3),
    size_bin = case_when(
      size_bin == 1 ~ "Small",
      size_bin == 2 ~ "Medium",
      size_bin == 3 ~ "Large"
    )
  )

# Merge back to full panel
con_stag_year2 <- con_stag_year2 %>%
  left_join(city_size_bin_lookup, by = "Place_Name_Clean")
names(which(table(con_stag_year2$Place_Name_Clean) != 37))

balance_table <-con_stag_year2 %>%
  group_by(adoption_group) %>%
  summarise(
    n = n(),
    population = mean(population, na.rm = TRUE),
    mf_permits_per_1000 = mean(mf_permits_per_1000, na.rm = TRUE),
    mf_log_permits_per_1000 = mean(mf_log_permits_per_1000,na.rm=T),
    sf_permits_per_1000 = mean(sf_permits_per_1000, na.rm = TRUE),
    sf_log_permits_per_1000 = mean(sf_log_permits_per_1000, na.rm = TRUE)
  )
print(balance_table,n=100)

summary(lm(mf_log_permits_per_1000 ~ factor(adoption_group) + population + factor(MUN_TYPE) + factor(Year) + factor(Place_Name_Clean), data = con_stag_year2))
ggplot(con_stag_year2, aes(x = adoption_group, y = mf_log_permits_per_1000)) + 
  geom_boxplot()

con_stag_year2$G <- ifelse(is.na(con_stag_year2$first_treatment_year), 0, con_stag_year2$first_treatment_year)
table(con_stag_year2$G)
con_stag_year2 <- con_stag_year2 %>%
  ungroup() %>%
  mutate(id = as.integer(as.factor(Place_Name_Clean)))

table(table(con_stag_year2$id))

cs_data <- con_stag_year2 %>%
  filter(!is.na(G)) %>% 
  mutate(
    time = as.numeric(Year),
    G = G,
    Y = log_mf_sum,
    Y2 = multi_family_sum
  )
names(which(table(cs_data$Place_Name_Clean) != 37))

cs_data <- cs_data %>%
  arrange(Place_Name_Clean,Year)%>%
  group_by(Place_Name_Clean)%>%
  mutate(event_time = ifelse(G > 0, Year - G, NA_integer_))%>%
  filter(is.na(event_time) | (event_time >= -10 & event_time <= 10))

table(table(cs_data$id))
table(cs_data$G)
#colSums(is.na(cs_data))
names(which(table(cs_data$Place_Name_Clean) != 37))

cs_data <- cs_data %>%
  mutate(
    G_binned = case_when(
      G == 0 ~ 0,
      G <= 2000 ~ 1,  # early adopters
      G <= 2010 ~ 2,  # mid adopters
      G > 2010 ~ 3    # late adopters
    )
  )

att_gt_results <- att_gt(
  yname = "single_family_sum",
  tname = "Year",
  idname = "id",
  gname = "G",
  xformla = ~ County_Name + MUN_TYPE + size_bin,
  control_group = "notyettreated",
  data = cs_data_class %>% filter(G==0|mf_class == "High MF"),
  #panel = T,
  allow_unbalanced_panel = T,
  est_method = "reg",
  anticipation = 1
)

es_results <- aggte(att_gt_results, type = "dynamic",na.rm = TRUE)
summary(es_results)
ggdid(es_results)

es_results_trimmed <- aggte(
  att_gt_results,
  type = "dynamic",
  min_e = -10,
  max_e = 10,
  na.rm = TRUE
)
summary(es_results_trimmed)
ggdid(es_results_trimmed)

library(dplyr)
library(stringr)

# Extract enactment and amendment years from RentControl

cs_data_filtered <- cs_data %>%
  arrange(Place_Name_Clean,Year)%>%
  group_by(Place_Name_Clean)%>%
  # Only keep never-treated or treated after 2000
  filter(G == 0 | (!is.na(first_amendment_year))) %>%
  # For treated units, drop years after the first amendment
  filter(G == 0 | Year <= (first_amendment_year - 1))%>%
  mutate(event_time = ifelse(G > 0, Year - G, NA_integer_))%>%
  filter(is.na(event_time) | (event_time >= -10 & event_time <= 10))

cs_data_filtered <- cs_data %>%
  arrange(Place_Name_Clean, Year) %>%
  group_by(Place_Name_Clean) %>%
  # Keep never-treated units or units treated after 2000 with amendment info
  filter(G == 0 | (!is.na(first_amendment_year))& Year < first_amendment_year) %>%
  mutate(event_time = ifelse(G > 0, Year - G, NA_integer_)) %>%
  filter(is.na(event_time) | (event_time >= -15 & event_time <= 15))

cs_data_filtered <- cs_data %>%
  arrange(Place_Name_Clean, Year) %>%
  group_by(Place_Name_Clean) %>%
  filter(adoption_group %in% c("Pre-2000", "Post-2000")) %>%
  filter(
    adoption_group == "Pre-2000" |
      (adoption_group == "Post-2000" & Year <= (first_amendment_year - 1))
  ) %>%
  mutate(
    G = if_else(adoption_group == "Pre-2000", 0L, G),
    event_time = if_else(G > 0, Year - G, NA_integer_)
  )%>%filter(is.na(event_time) | (event_time >= -10 & event_time <= 10))

table(cs_data_filtered$G == 0, useNA = "always")
table(cs_data_filtered$time < cs_data_filtered$G, useNA = "always")
table(cs_data_filtered$G, cs_data_filtered$time < cs_data_filtered$G)

range(cs_data_filtered$event_time,na.rm=T)
cs_data_filtered %>%
  group_by(id) %>%
  summarise(
    n_years = n_distinct(time),
    min_year = min(time),
    max_year = max(time),
    G = first(G)
  ) %>%
  ungroup() %>%
  filter(G > 0, min_year < G) %>%
  summarise(
    avg_years = mean(n_years),
    min_years = min(n_years),
    max_years = max(n_years)
  )
table(cs_data_filtered$G == 0, useNA = "always")
table(cs_data_filtered$time - cs_data_filtered$G)

cs_data_filtered %>%
  filter(!is.na(event_time), G > 0) %>%
  count(event_time) %>%
  ggplot(aes(x = event_time, y = n)) +
  geom_col() +
  labs(title = "Number of Treated Units by Event Time",
       x = "Event Time", y = "Number of Units")

cs_data_filtered %>%
  filter(G > 0) %>%
  group_by(id, G) %>%
  summarise(has_pre_period = any(Year < G),
            f1_year = first(Year),
            f1_treat = first(first_treatment_year),
            f1_amend =first(first_amendment_year),
            .groups = "drop") %>%
  filter(!has_pre_period)

att_gt_first_stage <- att_gt(
  yname = "Y",
  tname = "time",
  idname = "id",
  gname = "G",
  xformla = ~ County_Name + MUN_TYPE + size_bin,
  control_group = "notyettreated",
  data = cs_data_filtered,
  #panel=T,
  allow_unbalanced_panel = TRUE,
  est_method = "reg"
  )

es_results_first_stage <- aggte(att_gt_first_stage, type = "dynamic", na.rm = TRUE)
summary(es_results_first_stage)
ggdid(es_results_first_stage)





#### Bacon decomp ----
all_ids <- unique(cs_data$id)
all_years <- unique(cs_data$time)

balanced_panel <- expand.grid(
  id = all_ids,
  time = all_years
)

cs_data_balanced <- balanced_panel %>%
  left_join(cs_data, by = c("id", "time")) %>%
  arrange(id, time) %>%
  group_by(id) %>%
  mutate(
    G = first(na.omit(G)),
    D = ifelse(!is.na(G) & G > 0 & time >= G, 1, 0),
    post = D
  ) %>%
  ungroup()

complete_ids <- cs_data_balanced %>%
  group_by(id) %>%
  summarise(missing_Y = any(is.na(Y))) %>%
  filter(!missing_Y) %>%
  pull(id)

cs_data_bacon_clean <- cs_data_balanced %>%
  filter(id %in% complete_ids)

cs_data_bacon_clean_treated <- cs_data_balanced %>%
  filter(G >0& is.na(rent_control_unweighted))
# Run Bacon decomposition
bacon_results <- bacon(
  formula = Y ~ post + rent_control_unweighted,
  data = cs_data_bacon_clean_treated,
  id_var = "id",
  time_var = "time"
)
beep()

print(bacon_results)
# Weighted average ATT
summary(bacon_results)


fit_tw <- lm(Y ~ post + factor(id) + factor(time), data = cs_data_bacon_clean)
print(paste("Two-way FE estimate =", round(coef(fit_tw)["post"], 4)))

coef_bacon <- sum(bacon_results$estimate * bacon_results$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

ggplot(bacon_results) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()
  
# agg_gt_binned <- aggte(att_gt_results, type = "group", group = cs_data$G_binned)
# summary(agg_gt_binned)


#### RCI ----

cs_rci_data <- con_stag_year2 %>%
  mutate(
    RCI_quartile = case_when(
      G > 2000 ~ ntile(rent_control_unweighted, 4),
      G < 2000 ~ ntile(rent_control_unweighted, 4),
      TRUE ~ NA_integer_
    )
  )

cs_rci_data <- cs_rci_data %>%
  mutate(
    total_rci_quartile = ntile(rent_control_unweighted, 4)
  )

cs_rci_data <- cs_rci_data %>%
  mutate(post2000 = ifelse(G > 2000, 1, 0))

summary(lm(mf_log_permits_per_1000 ~ factor(total_rci_quartile)*post2000 + factor(Year) + MUN_TYPE+County_Name+factor(size_bin), cs_rci_data))
summary(lm(mf_log_permits_per_1000 ~ factor(RCI_quartile)+ factor(Year) + MUN_TYPE+County_Name+factor(size_bin), cs_rci_data%>% filter(G > 2000)))
summary(lm(mf_log_permits_per_1000 ~ factor(RCI_quartile)+ factor(Year)+MUN_TYPE + County_Name+factor(size_bin), cs_rci_data%>% filter(G < 2000)))

ggplot(cs_rci_data %>% filter(G < 2000|G==0), aes(x = Year, y = mf_log_permits_per_1000, group=factor(RCI_quartile), color = factor(RCI_quartile))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Multifamily Development Trends",
    subtitle = "Comparison of pre-2000, post-2000, and never-treated",
    x = "Year",
    y = "mf_log_permits_per_1000",
    color = "adoption_group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

cs_rci_data %>%
  filter(!is.na(rent_control_unweighted), !is.na(G)) %>%
  mutate(
    post2000 = ifelse(G > 2000, "Post-2000", "Pre-2000"),
    post2000 = factor(post2000, levels = c("Pre-2000", "Post-2000"))
  ) %>%
  group_by(post2000) %>%
  mutate(mean_rci = mean(rent_control_unweighted, na.rm = TRUE)) %>%
  ggplot(aes(x = rent_control_unweighted)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_rci), linetype = "dashed", color = "red") +
  geom_text(
    aes(x = mean_rci, y = 0, label = paste0("Mean = ", round(mean_rci, 2))),
    vjust = -30,
    hjust = -0.1,
    color = "red",
    size = 4
  ) +
  facet_wrap(~ post2000, scales = "free_y") +
  labs(
    title = "Distribution of Rent Control Intensity (RCI)",
    x = "Unweighted RCI",
    y = "Count"
  ) +
  theme_minimal()


##### ammendments ----

cs_rci_data_amend <- cs_rci_data %>%
  filter(!is.na(RentControl)) %>%
  group_by(Place_Name_Clean) %>%
  summarise(
    RentControl_combined = first(RentControl),  # assuming it's the same per city
    rent_control_unweighted = first(rent_control_unweighted),
    total_rci_quartile = first(total_rci_quartile),
    G = first(G)
  ) %>%
  mutate(
    RentControl_list = str_split(RentControl_combined, ";"),
    AmendYears = lapply(RentControl_list, function(x) {
      years <- str_extract(x, "\\d{4}")
      years <- years[!is.na(years)]
      as.integer(unique(years))
    }),
    num_amendments = sapply(AmendYears, length),
    first_year = sapply(AmendYears, min, na.rm = TRUE),
    last_year = 2024,
    duration_years = last_year - first_year + 1,
    amendment_rate = num_amendments / duration_years,
    post2000 = ifelse(G > 2000, "Post-2000", "Pre-2000")
  )

cs_rci_data_amend %>%
  filter(!is.na(amendment_rate), !is.na(G)) %>%
  mutate(
    post2000 = ifelse(G > 2000, "Post-2000", "Pre-2000"),
    post2000 = factor(post2000, levels = c("Pre-2000", "Post-2000"))
  ) %>%
  group_by(post2000) %>%
  mutate(mean_rci = mean(amendment_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = amendment_rate)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_rci), linetype = "dashed", color = "red") +
  geom_text(
    aes(x = mean_rci, y = 0, label = paste0("Mean = ", round(mean_rci, 2))),
    vjust = -1,
    hjust = -0.1,
    color = "red",
    size = 4
  ) +
  facet_wrap(~ post2000, scales = "free_y") +
  labs(
    title = "Distribution of Amendments",
    x = "rate of amendements",
    y = "Count"
  ) +
  theme_minimal()


cs_rci_data_amend <- cs_rci_data_amend %>%
  filter(!is.na(amendment_rate), !is.na(G)) %>%
  mutate(
    post2000 = ifelse(G > 2000, "Post-2000", "Pre-2000"),
    post2000 = factor(post2000, levels = c("Pre-2000", "Post-2000"))
  )

ggplot(cs_rci_data_amend, aes(x = log(rent_control_unweighted), y = log(amendment_rate))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = T) +
  facet_wrap(~post2000, scales = "free")
  labs(
    title = "Rate of Amendments vs. RCI",
    x = "Rent Control Intensity (RCI)",
    y = "Rate of Amendments"
  ) +
  theme_minimal()

summary(lm(log(amendment_rate)~log(rent_control_unweighted)*post2000,cs_rci_data_amend))
summary(lm(log(amendment_rate)~factor(total_rci_quartile)*post2000,cs_rci_data_amend))

total_rci_quartile

rc_amendment_years <- con_stag_year2 %>%
  filter(G > 2000 & !is.na(RentControl)) %>%
  distinct(Place_Name_Clean, RentControl) %>%
  mutate(
    years = str_split(RentControl, ";"),
    years = lapply(years, function(x) str_extract(x, "\\d{4}")),
    years = lapply(years, function(x) as.integer(na.omit(unique(x))))
  ) %>%
  unnest_longer(years) %>%
  distinct(years) %>%
  filter(years >= 2000, years <= 2024)

ggplot(con_stag_year2 %>% filter(G > 2000 & !is.na(RentControl)), aes(x = Year, y = mf_log_permits_per_1000, group = factor(Place_Name_Clean), color = factor(Place_Name_Clean))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  geom_vline(data = rc_amendment_years, aes(xintercept = as.character(years)), linetype = "dashed", color = "black", alpha = 0.6) +
  labs(
    title = "Multifamily Development Trends",
    x = "Year",
    y = "mf_log_permits_per_1000",
    color = "Adoption Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

rc_amendments <- con_stag_year2 %>%
  filter(G > 2000 & !is.na(RentControl)) %>%
  select(Place_Name_Clean, RentControl) %>%
  mutate(amend_years = str_extract_all(RentControl, "\\d{4}")) %>%
  unnest(amend_years) %>%
  mutate(amend_years = as.integer(amend_years))%>%
  distinct(Place_Name_Clean,amend_years)

# Keep only amendment years in the panel
amendment_panel <- con_stag_year2 %>%
  filter(G > 2000) %>%
  mutate(id = Place_Name_Clean) %>%
  left_join(rc_amendments, by = c("id" = "Place_Name_Clean"),relationship = "many-to-many") %>%
  mutate(is_amend_year = Year == amend_years) %>%
  filter(!is.na(amend_years)) %>%
  mutate(varname = paste0("amend_", amend_years)) %>%
  select(id, Year, varname, is_amend_year)

# Spread to wide: one dummy column per amendment year
amendment_dummies <- amendment_panel %>%
  pivot_wider(names_from = varname, values_from = is_amend_year, values_fill = 0)

# Merge dummies into the main panel
cs_post2000 <- con_stag_year2 %>%
  filter(G > 2000) %>%
  mutate(id = Place_Name_Clean) %>%
  left_join(amendment_dummies, by = c("id", "Year"))
names(cs_post2000)

feols(
  lead(mf_log_permits_per_1000,1) ~ 
    amend_2001 + amend_2002 + amend_2003 + amend_2004 + amend_2005 +
    amend_2006 + amend_2007 + amend_2008 + amend_2009 + amend_2010 +
    amend_2011 + amend_2012 + amend_2014 + amend_2015 + amend_2016 +
    amend_2017 + amend_2018 + amend_2019 + amend_2020 + amend_2021 +
    amend_2022 + amend_2023 + amend_2024 |
    factor(id) + factor(Year),
  data = cs_post2000
) |> summary()


event_time_panel <- con_stag_year2 %>%
  filter(G > 2000 & !is.na(RentControl)) %>%
  select(id = Place_Name_Clean, Year, mf_log_permits_per_1000, RentControl) %>%
  mutate(amend_years = str_extract_all(RentControl, "\\d{4}")) %>%
  unnest(amend_years) %>%
  mutate(amend_years = as.integer(amend_years)) %>%
  filter(as.numeric(Year) >= (amend_years - 5) & as.numeric(Year) <= (amend_years + 5)) %>%
  mutate(event_time = as.numeric(Year) - amend_years) %>%
  select(id, Year, event_time, mf_log_permits_per_1000)

library(fixest)

event_study_model <- feols(
  mf_log_permits_per_1000 ~ i(event_time, ref = -1) | factor(id) + factor(Year),
  data = event_time_panel
)
summary(event_study_model)

iplot(event_study_model, ref.line = 0, main = "Event study: Amendments and MF Permits")

event_time_panel_first <- con_stag_year2 %>%
  filter(G > 2000 & !is.na(RentControl)) %>%
  select(id = Place_Name_Clean, Year, mf_log_permits_per_1000, RentControl) %>%
  mutate(amend_years = lapply(str_extract_all(RentControl, "\\d{4}"), function(x) {
    sort(unique(as.integer(x)))
  })) %>%
  mutate(first_amend_year = sapply(amend_years, function(x) if (length(x) > 1) x[2] else NA_integer_)) %>%
  filter(!is.na(first_amend_year)) %>%
  filter(as.numeric(Year) >= (first_amend_year - 5) & as.numeric(Year) <= (first_amend_year + 5)) %>%
  mutate(event_time = as.numeric(Year) - first_amend_year) %>%
  select(id, Year, event_time, mf_log_permits_per_1000)

event_study_model_first <- feols(
  mf_log_permits_per_1000 ~ i(event_time, ref = -1) | factor(id) + factor(Year),
  data = event_time_panel_first
)
summary(event_study_model_first)
iplot(event_study_model_first, ref.line = 0, main = "Event study: Amendments and MF Permits")

rent_timing <- cs_rci_data %>%
  filter(!is.na(RentControl)) %>%
  mutate(
    # Extract all years as numeric
    RentControl_years = str_extract_all(RentControl, "\\d{4}"),
    RentControl_years = lapply(RentControl_years, function(x) as.integer(x)),
    
    # Find first two unique sorted years
    sorted_years = lapply(RentControl_years, function(x) sort(unique(x))),
    enact_year = sapply(sorted_years, function(x) if (length(x) > 0) x[1] else NA_integer_),
    first_amend_year = sapply(sorted_years, function(x) if (length(x) > 1) x[2] else NA_integer_),
    
    # Difference between first amendment and enactment
    years_to_first_amendment = first_amend_year - enact_year
  )

# Get average wait time (omit NA values for those with no amendment)
avg_gap <- mean(rent_timing$years_to_first_amendment, na.rm = TRUE)
avg_gap

rent_timing %>%
  mutate(post2000 = ifelse(G > 2000, "Post-2000", "Pre-2000")) %>%
  group_by(post2000) %>%
  summarise(
    avg_years_to_amend = mean(years_to_first_amendment, na.rm = TRUE),
    n = sum(!is.na(years_to_first_amendment))
  )


amendments_long <- con_stag_year2 %>%
  filter(G > 2000 & !is.na(RentControl)) %>%
  select(id = Place_Name_Clean, G, RentControl) %>%
  mutate(amend_years = str_extract_all(RentControl, "\\d{4}")) %>%
  unnest(amend_years) %>%
  mutate(amend_years = as.integer(amend_years)) %>%
  arrange(id, amend_years) %>%
  group_by(id) %>%
  mutate(amendment_number = row_number()) %>%
  ungroup()

# Step 4: Create event-time panel for each amendment-year
event_time_panel <- con_stag_year2 %>%
  filter(G > 2000 & !is.na(RentControl)) %>%
  select(id = Place_Name_Clean, Year, mf_log_permits_per_1000) %>%
  left_join(amendments_long, by = "id",relationship = "many-to-many") %>%
  filter(as.numeric(Year) >= amend_years - 5 & Year <= amend_years + 5) %>%
  mutate(event_time = as.numeric(Year) - amend_years)

# Step 5: Run separate event study models
first_amend_model <- feols(
  mf_log_permits_per_1000 ~ i(event_time, ref = -1),
  data = event_time_panel %>% filter(amendment_number == 1)
)

later_amend_model <- feols(
  mf_log_permits_per_1000 ~ i(event_time, ref = -1) | factor(id) + factor(Year),
  data = event_time_panel %>% filter(amendment_number > 1)
)

# Summaries
summary(first_amend_model)
summary(later_amend_model)






#### Later work ----
amendment_cols <- c(
  "RentControl", "RentIncreaseLimit", "Exemptions", "UnitsStructure",
  "VacancyIncrease", "VacancyControl", "HardshipIncreases", "CapitalImprovements",
  "AdministrativeProcedures", "EvictionControls", "RegistrationRequirements",
  "FeeSchedules", "ExpirationOrSunset", "Definitions"
)

amendments_long_unique <- con_stag_year %>%
  select(Place_Name_Clean, all_of(amendment_cols)) %>%
  pivot_longer(cols = -c(Place_Name_Clean), names_to = "Component", values_to = "Dates") %>%
  filter(!is.na(Dates)) %>%
  mutate(Date = str_split(Dates, ";")) %>%
  unnest(Date) %>%
  mutate(
    Date = suppressWarnings(ymd(str_trim(Date))),
    AmendmentYear = year(Date)
  ) %>%
  filter(!is.na(Date)&Component == "RentControl") %>%
  distinct(Place_Name_Clean, Component, AmendmentYear, .keep_all = TRUE) 


amendment_event_panel <- con_stag_year %>%
  inner_join(
    amendments_long %>% distinct(Place_Name_Clean, AmendmentYear),
    by = "Place_Name_Clean", relationship = "many-to-many"
  ) 

head(amendment_event_panel)
names(amendment_event_panel)
unique(amendment_event_panel$first_treatment_year)

amendment_event_panel_pre_2000 <- amendment_event_panel %>%
  group_by(Place_Name_Clean, Year) %>%
  filter(min(AmendmentYear, na.rm = TRUE) < 2000) %>%
  ungroup()

amendment_event_panel <- amendment_event_panel %>%
  mutate(
    adoption_group = case_when(
      first_treatment_year < 2000 ~ "Pre-2000",
      first_treatment_year >= 2000 ~ "Post-2000",
      TRUE ~ NA_character_
    )
  )

amendment_years_summary <- amendment_event_panel %>%
  select(Place_Name_Clean, AmendmentYear) %>%
  distinct() %>%
  arrange(Place_Name_Clean, AmendmentYear) %>%
  group_by(Place_Name_Clean) %>%
  summarise(AmendmentYears = list(sort(unique(AmendmentYear))), .groups = "drop")

pre2000_cities <- amendment_event_panel %>%
  filter(AmendmentYear < 2000) %>%
  pull(Place_Name_Clean) %>%
  unique()


print(unique(amendment_event_panel$adoption_group))
amendment_event_panel <- amendment_event_panel %>%
  arrange(Place_Name_Clean, Year)%>%
  group_by(Place_Name_Clean)%>%
  mutate(log_mf_sum_lead1 = lead(log_mf_sum, n = 1),
         log_mf_sum_per_1000_lead1 = lead(mf_log_permits_per_1000, n = 1))

plot_data <- amendment_event_panel %>%
  group_by(Year, adoption_group) %>%
  summarise(mean_log_mf_sum = mean(log_mf_sum, na.rm = TRUE),
            mean_mf_log_permits_per_1000_sum = mean(mf_log_permits_per_1000, na.rm = TRUE),
            .groups = "drop")

amendment_event_panel <- amendment_event_panel %>%
  mutate(
    amendment_early = as.integer(AmendmentYear >= 2001 & AmendmentYear <= 2008),
    amendment_middle = as.integer(AmendmentYear >= 2009 & AmendmentYear <= 2016),
    amendment_late = as.integer(AmendmentYear >= 2017 & AmendmentYear <= 2025)
  )

summary(lm(log_mf_sum_per_1000_lead1~factor(adoption_group)*factor(amendment_early) +factor(adoption_group)*factor(amendment_middle)+ factor(adoption_group)*factor(amendment_late)+ factor(Year) + factor(Place_Name_Clean),amendment_event_panel))

amendment_lines_all <- amendment_event_panel %>%
  #filter(Place_Name_Clean %in% first_five_cities) %>%
  distinct(Place_Name_Clean, AmendmentYear)%>%
  filter(AmendmentYear > 2000)

significant_years <- c(
  2001, 2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
  2012, 2013, 2015, 2018, 2020, 2021, 2023, 2025
)

ggplot(plot_data, aes(x = as.numeric(Year), y = mean_mf_log_permits_per_1000_sum, color = adoption_group)) +
  geom_line(size = 1.2) +
  geom_vline(
    xintercept = significant_years,
    linetype = "dashed",
    color = "black",
    alpha = 0.6
  )+
  labs(
    title = "Average Log Multi-Family Permitting by Adoption Group",
    x = "Year",
    y = "Average log(MF Permits)",
    color = "Adoption Group"
  ) +
  theme_minimal()

first_five_cities <- amendment_years_summary %>%
  slice_head(n = 5) %>%
  pull(Place_Name_Clean)

plot_data <- amendment_event_panel %>%
  filter(Place_Name_Clean %in% first_five_cities) %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(multi_family_sum = first(multi_family_sum),
            log_mf_sum = first(log_mf_sum),
            log_sf_sum = first(log_sf_sum),
            mf_log_permits_per_1000 = first(mf_log_permits_per_1000), 
            .groups = "drop")%>%
  mutate(Year = as.numeric(Year))

amendment_lines <- amendment_event_panel %>%
  filter(Place_Name_Clean %in% first_five_cities) %>%
  distinct(Place_Name_Clean, AmendmentYear)%>%
  filter(AmendmentYear > 2000)


ggplot(plot_data, aes(x = Year, y = mf_log_permits_per_1000, group = Place_Name_Clean)) +
  geom_line() +
  geom_vline(
    data = amendment_lines,
    mapping = aes(xintercept = AmendmentYear, group = Place_Name_Clean),
    linetype = "dashed",
    color = "red"
  ) +
  facet_wrap(~Place_Name_Clean, scales = "free_y") +
  labs(x = "Year", y = "Multi-Family Permits", title = "Multi-Family Permitting and Rent Control Amendments") +
  theme_minimal()

amendment_event_wide <- amendment_event_panel %>%
  distinct(Place_Name_Clean, Year, multi_family_sum,log_mf_sum,mf_log_permits_per_1000, AmendmentYear) %>%
  filter(AmendmentYear > 2000)%>%
  mutate(amendment_flag = 1) %>%
  mutate(log_mf_sum_lead1 = lead(log_mf_sum, n = 1),
         log_mf_sum_per_1000_lead1 = lead(mf_log_permits_per_1000,n=1))   %>%
  pivot_wider(
    names_from = AmendmentYear,
    values_from = amendment_flag,
    values_fill = 0,
    names_prefix = "AmendmentYear_"
  )

amendment_cols <- names(amendment_event_wide) %>%
  stringr::str_subset("^AmendmentYear_")

# Other control variables to include
control_vars <- c("Place_Name_Clean")  # add more like "SomeOtherVar", "Place_Name_Clean" if desired

# Construct formula
lm_formula <- as.formula(
  paste("log_mf_sum_per_1000_lead1 ~", paste(c(control_vars, amendment_cols), collapse = " + "))
)

# Run linear model
summary(lm(lm_formula, data = amendment_event_wide))
coeftest(amendment_lm, vcov = vcovCL, cluster = amendment_event_wide$Place_Name_Clean)

amendment_distribution <- amendment_event_panel %>%
  filter(AmendmentYear >2000) %>%
  distinct(Place_Name_Clean, AmendmentYear) %>%
  filter(!is.na(AmendmentYear)) %>%
  count(AmendmentYear) %>%
  arrange(AmendmentYear)

ggplot(amendment_distribution %>% filter(AmendmentYear >2000), aes(x = AmendmentYear, y = n)) +
  geom_col() +
  labs(
    title = "Distribution of Rent Control Amendments Over Time",
    x = "Amendment Year",
    y = "Number of Cities"
  ) +
  theme_minimal()

amendment_event_wide <- amendment_event_wide %>%
  mutate(
    amendment_early = rowSums(across(paste0("AmendmentYear_", 2001:2008)), na.rm = TRUE) > 0,
    amendment_middle = rowSums(across(paste0("AmendmentYear_", 2009:2016)), na.rm = TRUE) > 0,
    amendment_late = rowSums(across(paste0("AmendmentYear_", 2017:2025)), na.rm = TRUE) > 0
  ) %>%
  mutate(
    amendment_early = as.integer(amendment_early),
    amendment_middle = as.integer(amendment_middle),
    amendment_late = as.integer(amendment_late),
    amendment_bin = case_when(
      amendment_early == 1 ~ "early",
      amendment_middle == 1 ~ "middle",
      amendment_late == 1 ~ "late",
      TRUE ~ "none"
    )
  )
amendment_event_wide <- amendment_event_wide %>%
  mutate(
    amendment_bin = case_when(
      amendment_early == 1 ~ "early",
      amendment_middle == 1 ~ "middle",
      amendment_late == 1 ~ "late",
      TRUE ~ "none"
    )
  )

lm_bins_fe <- lm(
  log_mf_sum_per_1000_lead1 ~ factor(Year) + factor(amendment_bin),
  data = amendment_event_wide
)
summary(lm_bins_fe)
coeftest(lm_bins_fe, vcov = vcovCL, cluster = amendment_event_wide$Place_Name_Clean)

lm_amendment_bins <- lm(
  log_mf_sum_per_1000_lead1 ~ factor(Year) + amendment_early + amendment_middle + amendment_late,
  data = amendment_event_wide
)
summary(lm_amendment_bins)
coeftest(lm_amendment_bins, vcov = vcovCL, cluster = amendment_event_wide$Place_Name_Clean)



#### Chronic updater vs stagnant ----


chronic_status <- amendment_event_panel %>%
  filter(first_treatment_year < 2000, !is.na(AmendmentYear)) %>%
  group_by(Place_Name_Clean,first_treatment_year) %>%
  summarise(
    n_amendments = n_distinct(AmendmentYear, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    years_active = 2024 - first_treatment_year + 1,
    amendments_per_year = n_amendments / years_active,
    amendment_pattern = if_else(amendments_per_year >= median(amendments_per_year), "high_rate_amender", "low_rate_amender"),
    amend_rate_quartile = ntile(amendments_per_year, 4)
  )
chronic_status
hist(chronic_status$amendments_per_year)
print(chronic_status,n=100)

# Merge back into main panel
amendment_event_panel_status <- amendment_event_panel %>%
  merge(chronic_status, by = "Place_Name_Clean")

summary(lm(log_mf_sum_per_1000_lead1~n_amendments* factor(Year) + factor(Place_Name_Clean),amendment_event_panel_status))
summary(lm(log_mf_sum_per_1000_lead1~amendments_per_year* factor(Year) + factor(Place_Name_Clean),amendment_event_panel_status))
summary(lm(log_mf_sum_per_1000_lead1~factor(amend_rate_quartile)* factor(Year) + factor(Place_Name_Clean),amendment_event_panel_status))

chronic_plot_data <- amendment_event_panel_status %>%
  filter(first_treatment_year.x < 2000, !is.na(amendment_pattern))

ggplot(chronic_plot_data, aes(x = Year, y = log_mf_sum_per_1000, group=factor(amend_rate_quartile), color = factor(amend_rate_quartile))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Multifamily Development Trends in Pre-2000 Cities",
    subtitle = "Comparison of Chronic Updaters vs Stagnant Cities",
    x = "Year",
    y = "Log(Multifamily Sum)",
    color = "Amendment Pattern"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


#### RCI vs chronic ----
rci_chronic_panel <- merge(chronic_status,rent_control_intensity,by.x ="Place_Name_Clean",by.y ="Municipality_Clean")
plot((rci_chronic_panel$amendments_per_year),(rci_chronic_panel$rent_control_unweighted))

rci_chronic_panel_binned <- rci_chronic_panel %>%
  mutate(
    rci_bin = cut(
      rent_control_unweighted,
      breaks = c(0, 0.25, 0.5,.75, 1),
      labels = c("0–0.25", "0.25–0.5","0.5-0.75", "0.75–1"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(rci_bin) %>%
  summarise(
    avg_amendments = mean(amendments_per_year, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
rci_chronic_panel_binned
ggplot(rci_chronic_panel_binned, aes(x = rci_bin, y = avg_amendments)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Number of Amendments by RCI Bin",
    x = "Rent Control Intensity (RCI) Bin",
    y = "Average Number of Amendments"
  ) +
  theme_minimal()


ggplot(rci_chronic_panel, aes(x = rent_control_unweighted, y = amendments_per_year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(
    title = "Number of Amendments vs. RCI",
    x = "Rent Control Intensity (RCI)",
    y = "Number of Amendments"
  ) +
  theme_minimal()

rci_chronic_panel_quartile <- rci_chronic_panel %>%
  mutate(
    rci_qtile = ntile(rent_control_unweighted, 4)
  ) %>%
  group_by(rci_qtile) %>%
  summarise(
    avg_amendments = mean(amendments_per_year, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(rci_chronic_panel_quartile, aes(x = factor(rci_qtile), y = avg_amendments)) +
  geom_col(fill = "seagreen") +
  labs(
    title = "Average Amendments by RCI Quartile",
    x = "RCI Quartile (1 = Lowest)",
    y = "Average Number of Amendments"
  ) +
  theme_minimal()

summary(lm((amendments_per_year) ~ rent_control_unweighted+I(rent_control_unweighted^2),rci_chronic_panel))
summary(lm(rent_control_unweighted ~ amendments_per_year + years_active, data = rci_chronic_panel))

ggplot(rci_chronic_panel, aes(x = factor(amend_rate_quartile), y = rent_control_unweighted)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.1, alpha = 0.6) +
  labs(
    title = "RCI by Amendment Frequency Quartile",
    x = "Amendment Rate Quartile",
    y = "Rent Control Intensity (RCI)"
  ) +
  theme_minimal()

summary(aov(rent_control_unweighted ~ factor(amend_rate_quartile), data = rci_chronic_panel))

ggplot(rci_chronic_panel, aes(x = amend_rate_quartile, y = rent_control_unweighted)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal()

#### Post-2000 adopters ----


##### Chronic vs stagnant ----
# Identify chronic vs stagnant updaters among post-2000 adopters
post_chronic_status <- amendment_event_panel %>%
  filter(first_treatment_year >= 2000, !is.na(AmendmentYear)) %>%
  group_by(Place_Name_Clean,first_treatment_year) %>%
  summarise(
    n_amendments = n_distinct(AmendmentYear, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    years_active = 2024 - first_treatment_year + 1,
    amendments_per_year = n_amendments / years_active,
    amendment_pattern = if_else(amendments_per_year >= median(amendments_per_year), "high_rate_amender", "low_rate_amender"),
    amend_rate_quartile = ntile(amendments_per_year, 4)
  )

print(post_chronic_status,n=110)
# Merge into the main event panel
amendment_event_panel_post <- amendment_event_panel %>%
  filter(first_treatment_year >= 2000) %>%
  left_join(post_chronic_status, by = "Place_Name_Clean")


ggplot(amendment_event_panel_post, aes(x = Year, y = mf_log_permits_per_1000, group=factor(amend_rate_quartile), color = factor(amend_rate_quartile))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Multifamily Development Trends in Post-2000 Cities",
    subtitle = "Comparison of Chronic Updaters vs Stagnant Cities",
    x = "Year",
    y = "Log(Multifamily Sum)",
    color = "Amendment Pattern"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



summary(lm(mf_log_permits_per_1000 ~ factor(amend_rate_quartile)* factor(Year) + factor(Place_Name_Clean), data = amendment_event_panel_post))


amendment_event_panel_post$Year <- as.numeric(as.character(amendment_event_panel_post$Year))
amendment_event_panel_post$first_treatment_year.x <- as.numeric(as.character(amendment_event_panel_post$first_treatment_year.x))
amendment_event_panel_post$event_time <- amendment_event_panel_post$Year - as.numeric(amendment_event_panel_post$first_treatment_year.x)


amendment_event_panel_post$event_time <- factor(amendment_event_panel_post$event_time)
amendment_event_panel_post$event_time <- relevel(amendment_event_panel_post$event_time, ref = "-3")

# amendment_flags <- amendment_event_panel_post |>
#   select(Place_Name_Clean, Year, AmendmentYear) |>
#   filter(!is.na(AmendmentYear)) |>
#   mutate(flag = 1L) |>
#   pivot_wider(
#     names_from = AmendmentYear,
#     names_prefix = "amendment_yr_",
#     values_from = flag,
#     values_fill = 0
#   )
# amendment_event_panel_post_wide <- amendment_event_panel_post |>
#   select(-AmendmentYear) |>
#   distinct(Place_Name_Clean, Year, .keep_all = TRUE) |>
#   left_join(amendment_flags, by = c("Place_Name_Clean", "Year"))
# 
# 
# model_event <-lm(formula, data = amendment_event_panel_post_wide)

model_event <- lm(
  mf_log_permits_per_1000 ~ event_time*factor(amend_rate_quartile)+n_amendments+ factor(Place_Name_Clean) + factor(Year),
  data = amendment_event_panel_post
)
summary(model_event)

event_coefs <- tidy(model_event) |> 
  filter(grepl("^event_time", term)) |> 
  mutate(event_time = as.numeric(gsub("event_time", "", term)))

ggplot(event_coefs, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(x = "Years Since First Amendment", y = "Effect on Log MF Permits per 1,000", 
       title = "Event Study: Post-2000 First Amendment Timing")

amendment_event_panel_post <- amendment_event_panel_post %>%
  mutate(rci_n = ntile(rent_control_unweighted,4))

model_event <- lm(
  mf_log_permits_per_1000 ~ event_time*factor(rci_n)+n_amendments+ factor(Place_Name_Clean) + factor(Year),
  data = amendment_event_panel_post
)
summary(model_event)
event_coefs <- tidy(model_event) |> 
  filter(grepl("^event_time", term)) |> 
  mutate(
    base = !grepl(":", term),  # base group (quartile 1)
    quartile = ifelse(base, "1",
                      gsub(".*factor\\(rci_n\\)([2-4])", "\\1", term)),
    event_time = as.numeric(gsub("event_time", "", gsub(":.*", "", term)))
  )

# Spread base coefficients to all quartiles
event_coefs_wide <- event_coefs |> 
  select(term, estimate, std.error, event_time, quartile) |> 
  group_by(event_time) |> 
  mutate(
    total_estimate = ifelse(quartile == "1", estimate,
                            estimate + estimate[quartile == "1"]),
    total_se = ifelse(quartile == "1", std.error,
                      sqrt(std.error^2 + std.error[quartile == "1"]^2))
  ) |> 
  ungroup()

ggplot(event_coefs_wide, aes(x = event_time, y = total_estimate, color = quartile)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = total_estimate - 1.96 * total_se,
                    ymax = total_estimate + 1.96 * total_se), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(x = "Years Since First Amendment",
       y = "Effect on Log MF Permits per 1,000",
       color = "Amendment Rate Quartile",
       title = "Event Study by Amendment Rate Quartile") +
  theme_minimal()

### using control group
amendment_event_panel_post_control <- amendment_event_panel %>%
  filter(first_treatment_year >= 2000| is.na(first_treatment_year)) %>%
  left_join(post_chronic_status, by = "Place_Name_Clean")

amendment_event_panel_post_control <- amendment_event_panel_post_control %>%
  mutate(treatment_group = ifelse(!is.na(first_treatment_year.x), 1, 0))

amendment_event_panel_post_control <- amendment_event_panel_post_control %>%
  mutate(first_treatment_year = as.numeric(first_treatment_year.x),
         event_time = ifelse(treatment_group == 1,
                             as.numeric(Year) - as.numeric(first_treatment_year),
                             NA))

model_data <- amendment_event_panel_post_control %>%
  filter(!is.na(event_time), event_time != -1)

model_staggered <- lm(
  sf_log_permits_per_1000 ~ factor(event_time) + factor(Place_Name_Clean) + factor(Year),
  data = model_data
)

event_coefs <- tidy(model_staggered) %>%
  filter(grepl("^factor\\(event_time\\)", term)) %>%
  mutate(event_time = as.numeric(gsub("factor\\(event_time\\)", "", term)))

ggplot(event_coefs, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(x = "Years Since First Amendment", 
       y = "Effect on Log MF Permits per 1,000", 
       title = "Event Study with Staggered Treatment and Controls") +
  theme_minimal()

did_data <- amendment_event_panel_post_control %>%
  rename(
    id_raw = Place_Name_Clean,
    time = Year,
    Y = mf_log_permits_per_1000,
    G = first_treatment_year
  ) %>%
  mutate(
    id = as.integer(factor(id_raw)),  # convert Place_Name_Clean to unique integers
    time = as.numeric(time),
    G = ifelse(is.na(G), 0, G)
  )

table(amendment_event_panel_post_control$first_treatment_year,amendment_event_panel_post_control$Year)

did_model <- att_gt(
  yname = "Y",
  tname = "time",
  idname = "id",
  gname = "G",
  data = did_data,
  panel = TRUE,
  control_group = "notyettreated",
  est_method = "reg"
)
agg_event <- aggte(did_model, type = "dynamic")

