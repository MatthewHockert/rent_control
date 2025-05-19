library(panelView)
library(ggplot2)

#### Step 1-  plotting the roll out ----

data(panelView)
panelview(turnout ~ policy_edr + policy_mail_in + policy_motor, 
          data = turnout, index = c("abb","year"), 
          xlab = "Year", ylab = "State")


table(cs_data_filtered$G)
table(table(cs_data_filtered$id))

panel_data <- cs_data_filtered %>%
  distinct(id, Year, .keep_all = TRUE)

table(panel_data$G)
table(table(panel_data$id))

panel_data <- panel_data %>%
  mutate(D = ifelse(G != 0, 1, 0),
         D = ifelse(G == 0, 0, D))

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

panelview(Y ~ D,
          data = panel_data,
          index = c("id", "Year"),
          type = "treat",
          by.timing = TRUE)

#### Step 2 - document treatment cohorts ----

cs_data_filtered %>%
  group_by(id) %>%
  summarize(treatment_year = min(G, na.rm = TRUE)) %>%
  mutate(treatment_year = ifelse(is.infinite(treatment_year), NA, treatment_year)) %>%
  count(treatment_year, name = "num_units") %>%
  arrange(treatment_year)

cohort_table <- cs_data_filtered %>%
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
  arrange(factor(G, levels = c("Never treated", sort(unique(panel_data$G)))))
cohort_table

kable(cohort_table,
     col.names = c("Treatment Cohort", "Number of Units", "Percent of Units in Cohort"),
     caption = "Table 1: Number of Units by Treatment Cohort Timing Group")


#### Step 3 - Pre-trends ----


panel_data_event <- cs_data_prep %>%
  filter(!is.na(G) & G != 0) %>%  # only ever-treated units
  mutate(event_time = T - G) 

panel_data_event %>%
  group_by(event_time) %>%
  summarise(mean_outcome = mean(Y, na.rm = TRUE)) %>%
  ggplot(aes(x = event_time, y = mean_outcome)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Pre-Treatment Trends in Property Value",
       x = "Event Time (Years Since Treatment)",
       y = "Mean log(EMV + 1)")



panel_data_event %>%
  group_by(G, year) %>%
  summarise(mean_outcome = mean(Y, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_outcome, color = as.factor(G))) +
  geom_line(size = 1) +
  facet_wrap(~G)%>%
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Pre-Treatment Trends by Cohort",
       x = "Event Time (Years Since Treatment)",
       y = "Mean log(EMV + 1)",
       color = "Treatment Cohort")



panel_data_cohorts <- cs_data %>%
  filter(!is.na(G) & G > 0) %>%  # Only ever-treated
  group_by(G, Year) %>%
  summarise(mean_outcome = mean(Y, na.rm = TRUE), .groups = "drop") %>%
  mutate(cohort_label = paste0(G, " cohort"))

never_treated <- panel_data %>%
  filter(G == 0) %>%
  group_by(Year) %>%
  summarise(mean_outcome = mean(Y, na.rm = TRUE), .groups = "drop") %>%
  mutate(cohort_label = "never treated", G = NA)

pre_treated <- panel_data %>%
  filter(G != 0 &G < Year) %>%
  group_by(Year) %>%
  summarise(mean_outcome = mean(Y, na.rm = TRUE), .groups = "drop") %>%
  mutate(cohort_label = "Pre Treated", G = NA)

plot_data <- bind_rows(panel_data_cohorts, never_treated, pre_treated)

ggplot(plot_data, aes(x = Year, y = mean_outcome, group = cohort_label)) +
  geom_line(color = "blue") +
  geom_vline(aes(xintercept = G), color = "red", linetype = "dashed") +
  facet_wrap(~ cohort_label, scales = "free" ) +
  theme_minimal()


#### Step 4 - Understand Treatment Assignment ----

panel_data <- cs_data_prep %>%
  mutate(treated = ifelse(!is.na(G) & G != 0, 1, 0))%>%
  filter((G != 0 & distance_m < 2500) | G == 0)

#names(panel_data)
panel_data %>%
  filter(T < G | is.na(G)) %>%  # pre-treatment only
  group_by(treated) %>%
  summarise(
    avg_size = mean(ACREAGE, na.rm = TRUE),
    avg_value = mean(REAL_EMV_log1p, na.rm = TRUE),
    avg_tax= mean(REAL_tax_log1p, na.rm = TRUE),
    avg_tax_share = mean(tax_share, na.rm = TRUE)
  )

model <- glm(treated ~ log(ACREAGE + 1) + REAL_EMV_log1p + tax_share,
             data = panel_data %>% filter(T == min(T)),
             family = binomial)
summary(model)







