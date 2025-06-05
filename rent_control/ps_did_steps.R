library(panelView)
library(ggplot2)

#### Step 1-  plotting the roll out ----

data(panelView)
panelview(turnout ~ policy_edr + policy_mail_in + policy_motor, 
          data = turnout, index = c("abb","year"), 
          xlab = "Year", ylab = "State")


table(cs_data$G)
table(table(cs_data$id))

panel_data <- cs_data %>%
  distinct(id, Year, .keep_all = TRUE)

table(panel_data$G)
table(table(panel_data$id))

panel_data <- panel_data %>%
  mutate(D = ifelse(G != 0, 1, 0),
         D = ifelse(G == 0, 0, D))

panel_data <- panel_data %>%
  mutate(D_post = ifelse(!is.na(G) & Year >= G, 1, 0))

panelview(Y ~1,
          data = panel_data,
          index = c("id", "Year"),
          type = "missing",
          by.timing = TRUE)

panelview(Y ~ 1,
          data = panel_data,
          index = c("id", "Year"), 
          type = "treat",
          by.timing = TRUE)

panelview(Y ~ D_post,
          data = panel_data,
          index = c("id", "Year"),
          type = "treat",
          by.timing = TRUE)


range(panel_data$Year)

first_year <- min(panel_data$Year)

treated <- panel_data %>%
  group_by(id)%>%
  filter(G !=0)

panelview(Y ~ D_post,
          data = treated,
          index = c("id", "Year"),
          type = "treat",
          by.timing = TRUE)


#### Step 2 - document treatment cohorts ----

cs_data %>%
  group_by(id) %>%
  summarize(treatment_year = min(G, na.rm = TRUE)) %>%
  mutate(treatment_year = ifelse(is.infinite(treatment_year), NA, treatment_year)) %>%
  count(treatment_year, name = "num_units") %>%
  arrange(treatment_year)%>%
  print(n=100)

cs_data_filtered %>%
  distinct(id, G) %>%
  mutate(G = ifelse(is.na(G), "Never treated", as.character(G))) %>%
  group_by(G) %>%
  summarise(
    num_units = n()
  ) %>%
  ungroup() %>%
  mutate(
    percent = round(100 * num_units / sum(num_units), 2)
  ) %>%
  arrange(factor(G, levels = c("Never treated", sort(unique(panel_data$G)))))%>%
  print(n=100)

cs_data_filtered %>%
  distinct(id, G) %>%
  mutate(G = ifelse(is.na(G), "0", as.character(G))) %>%
  group_by(G) %>%
  summarise(num_units = n(), .groups = "drop") %>%
  arrange(as.numeric(G)) %>%
  mutate(
    cum_units = cumsum(if_else(G == "0", 0L, num_units)),
    cum_units = if_else(G == "0", num_units, cum_units),
    total_units = sum(num_units),
    cum_percent = round(100 * cum_units / total_units, 2)
  ) %>%
  select(G, num_units = cum_units, percent = cum_percent) %>%
  print(n = 100)



#### Step 3 - Pre-trends ----


panel_data_event <- cs_data %>%
  filter(G != 0) %>%  # only ever-treated units
  filter(G >1950)%>%
  mutate(event_time = Year - G) 

table(panel_data_event$G)

plot_nj_ag_permits <- panel_data_event %>%
  group_by(event_time) %>%
  summarise(mf_outcome = mean(log_mf_sum, na.rm = TRUE),
            mf_sum = sum(multi_family_sum, na.rm = TRUE),
            sf_sum = sum(single_family_sum, na.rm = TRUE)) 


ggplot(plot_nj_ag_permits, aes(x = event_time, y = mf_outcome)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Pre-Treatment Trends",
       x = "Event Time (Years Since Treatment)",
       y = "Mean log(permits)")


plot_nj_ag_permits <- nj_all2 %>%
  group_by(Year) %>%
  summarise(mf_sum = sum(multi_family, na.rm = TRUE),
            sf_sum = sum(single_family, na.rm = TRUE)) 

plot_nj_ag_permits_long <- plot_nj_ag_permits %>%
  pivot_longer(cols = c(mf_sum, sf_sum), names_to = "outcome_type", values_to = "value")

ggplot(plot_nj_ag_permits_long, aes(x = Year, y = value, color = outcome_type)) +
  geom_line() +
  theme_minimal() +
  labs(color = "Outcome")


panel_data_event %>%
  group_by(G, Year) %>%
  summarise(mf_outcome = mean(log_mf_sum, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mf_outcome, color = as.factor(G))) +
  geom_line(size = 1) +
  facet_wrap(~G)%>%
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Pre-Treatment Trends by Cohort",
       x = "Event Time (Years Since Treatment)",
       y = "Mean log(EMV + 1)",
       color = "Treatment Cohort")



panel_data_cohorts <- cs_data %>%
  filter(!is.na(G) & G > 0 & G != 1006) %>%  # Only ever-treated
  group_by(G, Year) %>%
  summarise(mf_outcome = mean(multi_family_sum, na.rm = TRUE),
            sf_outcome = mean(single_family_sum, na.rm = TRUE),
            G = first(G),
            .groups = "drop") %>%
  mutate(cohort_label = paste0(G, " cohort"))

never_treated <- cs_data %>%
  filter(G == 0) %>%
  group_by(Year) %>%
  summarise(mf_outcome = mean(multi_family_sum, na.rm = TRUE),
            sf_outcome = mean(single_family_sum, na.rm = TRUE),
            G = first(G),
            .groups = "drop") %>%
  mutate(cohort_label = "never treated", G = NA)

pre_treated <- cs_data %>%
  filter(G != 0 &G < Year & G >1950) %>%
  group_by(Year) %>%
  summarise(mf_outcome = mean(multi_family_sum, na.rm = TRUE),
            sf_outcome = mean(single_family_sum, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(cohort_label = "Pre Treated", G = NA)

plot_data <- bind_rows(panel_data_cohorts, never_treated, pre_treated)
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(sf_outcome, mf_outcome), names_to = "outcome_type", values_to = "value")

ggplot(plot_data, aes(x = Year, y = sf_outcome)) +
  geom_line(color = "blue") +
  geom_vline(aes(xintercept = G), color = "red", linetype = "dashed") +
  facet_wrap(~ cohort_label, scales = "free") +
  theme_minimal()

ggplot(plot_data_long, aes(x = Year, y = value, color = outcome_type)) +
  geom_line() +
  geom_vline(aes(xintercept = G), color = "black", linetype = "dashed") +
  facet_wrap(~ cohort_label, scales = "free") +
  theme_minimal() +
  labs(color = "Outcome")

drop_cohorts <- cs_data %>%
  filter(G == 2015 | G ==2017)%>%
  pull((Place_Name_Clean))
drop_cohorts


cs_data %>%
  filter(G == 2016)

cs_data$multi_family_sum
pre_treatment_avg <- cs_data %>%
  filter(Year < G) %>% 
  group_by(Place_Name_Clean) %>%
  summarise(
    G = first(G),
    avg_mf_permits = mean(multi_family_sum, na.rm = TRUE),
    n_years = n()
  ) %>%
  ungroup()

threshold <- median(pre_treatment_avg$avg_mf_permits, na.rm = TRUE)
threshold

pre_treatment_avg <- pre_treatment_avg %>%
  mutate(
    mf_class = if_else(avg_mf_permits > threshold, "High MF", "Low MF")
  )
cs_data_class <- cs_data %>% # Go back to model in amendment_timing_effects
  left_join(pre_treatment_avg %>% select(Place_Name_Clean, mf_class), by = "Place_Name_Clean")

table(cs_data[cs_data$mf_class == "High MF", ]$G)

threshold
cs_data_class %>%
  arrange(Place_Name_Clean,year)%>%
  filter(Year < G) %>% 
  group_by(G) %>%
  summarise(avg_mf_permits = mean(multi_family_sum, na.rm = TRUE)) %>%
  arrange(G)%>%
  print(n=100)

cs_data_class %>%
  filter(Year < G)%>%
  arrange(G)%>%
  pull(G)%>%
  unique()

cs_data %>%
  filter(G>0)%>%
  arrange(G)%>%
  pull(G)%>%
  unique()

cs_data %>%
  group_by(G) %>%
  summarise(min_year = min(Year), max_year = max(Year)) %>%
  filter(min_year >= G)%>%
  print(n=100)

table(cs_data_class$mf_class, cs_data_class$size_bin)
table(cs_data_class$mf_class, cs_data_class$MUN_TYPE)
table(cs_data_class$mf_class, cs_data_class$Place_Name_Clean)

cs_data_class %>%
  count(mf_class, size_bin) %>%
  group_by(mf_class) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(mf_class, desc(prop))

#### Step 4 - Understand Treatment Assignment ----










