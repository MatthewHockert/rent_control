library(fixest)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(readxl)

fannie_freddie_finances <- read_csv("/Users/matthewhockert/Downloads/FNMA_MF_Loan_Performance_Data_202412.csv")
fannie_freddie_finances$`Property City`
fannie_freddie_finances$`Issue Date`
names(fannie_freddie_finances)

fannie_freddie_finances <- fannie_freddie_finances %>%
  mutate(city_group = case_when(
    `Property City` == "SAINT PAUL" & `Property State` == "MN" ~ "Saint Paul",
    `Property City` == "MINNEAPOLIS" & `Property State` == "MN" ~ "Minneapolis",
    `Property City` == "PORTLAND" & `Property State` == "ME" ~ "Portland",
    TRUE ~ "Everyone Else"
  ))

fannie_freddie_finances_rc <- fannie_freddie_finances %>%
  mutate(Year = lubridate::year(`Note Date`),
         post = ifelse(Year >= 2021, 1, 0),
         treated = ifelse(city_group %in% c("Saint Paul", "Minneapolis"), 1, 0))

ggplot(fannie_freddie_finances_rc %>% filter(Year >2000), aes(x = Year, y = `Loan Acquisition LTV`, color = city_group)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") + # e.g., policy approval
  labs(title = "LTV Ratios Over Time", y = "Loan-to-Value Ratio", x = "Year") +
  theme_minimal()

fannie_freddie_finances_rc <- fannie_freddie_finances_rc %>%
  filter(`Underwritten DSCR` < 10)
ggplot(fannie_freddie_finances_rc%>% filter(Year >2000), aes(x = factor(post), y = `Underwritten DSCR`, fill = city_group)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Pre-2021", "Post-2021")) +
  labs(title = "DSCR Pre vs. Post Rent Control Debate", x = "Policy Period", y = "DSCR") +
  theme_minimal()
names(fannie_freddie_finances_rc) <- gsub("[^A-Za-z0-9]+", "_", names(fannie_freddie_finances_rc))
names(fannie_freddie_finances_rc)
summary(feols((`Underwritten_DSCR`) ~ treated * post + city_group +
                Loan_Acquisition_LTV +
                Loan_Product_Type +
                Property_Acquisition_Total_Unit_Count
              |Property_City+ factor(Year) + factor(`Metropolitan_Statistical_Area`),
              data = fannie_freddie_finances_rc)
)
beep()
ggplot(fannie_freddie_finances_rc%>%filter(Year >2000), aes(x = Year, y = `Original Interest Rate`, color = city_group)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
  labs(title = "Interest Rates Over Time", y = "Original Interest Rate", x = "Year") +
  theme_minimal()

fannie_freddie_finances_nj <-subset(fannie_freddie_finances,`Property State` == "NJ")
print(unique(fannie_freddie_finances_nj$`Property City`))
length(unique(fannie_freddie_finances_nj$`Property City`))

municipalities <- read_excel("NJ_Rent_Control_Survey.xlsx", sheet = "Munis by Rent Control Status")

# Clean and standardize names
municipalities_clean <- municipalities %>%
  mutate(muni_clean = str_to_upper(Municipality),
         muni_clean = str_replace_all(muni_clean, "\\s+(CITY|TOWNSHIP|BOROUGH|TOWN|VILLAGE)$", ""),
         muni_clean = str_replace_all(muni_clean, "\\s+", " "),
         muni_clean = str_trim(muni_clean))

ff_cities <- fannie_freddie_finances_rc %>%
  select(PropertyCity = `Property City`) %>%
  distinct() %>%
  mutate(city_clean = str_to_upper(PropertyCity),
         city_clean = str_replace_all(city_clean, "\\s+", " "),
         city_clean = str_trim(city_clean))

# Perform fuzzy join
matched <- stringdist_left_join(ff_cities, municipalities_clean, 
                                by = c("city_clean" = "muni_clean"), 
                                method = "jw", max_dist = 0.15)

names(fannie_freddie_finances_nj)
names(matched)

matched_rent_control <- matched %>%
  select(PropertyCity, `Rent Control Ordinance?`) %>%
  distinct(PropertyCity, .keep_all = TRUE)

fannie_with_rent_control <- fannie_freddie_finances_rc %>%
  left_join(matched_rent_control, by = c("Property City" = "PropertyCity"))

ggplot(fannie_with_rent_control %>% filter(Year >2000), aes(x = Year, y = `Loan Acquisition LTV`, color = `Rent Control Ordinance?`)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") + # e.g., policy approval
  labs(title = "LTV Ratios Over Time", y = "Loan-to-Value Ratio", x = "Year") +
  theme_minimal()

ggplot(fannie_with_rent_control%>% filter(Year >2000), aes(x = factor(`Rent Control Ordinance?`), y = `Underwritten DSCR`, fill = `Rent Control Ordinance?`)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Yes", "No")) +
  labs(title = "DSCR Pre vs. Post Rent Control Debate", x = "Policy Period", y = "DSCR") +
  theme_minimal()


ggplot(fannie_with_rent_control%>%filter(Year >2000), aes(x = Year, y = `Original Interest Rate`, color = `Rent Control Ordinance?`)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
  labs(title = "Interest Rates Over Time", y = "Original Interest Rate", x = "Year") +
  theme_minimal()

sum(is.na(fannie_with_rent_control$`Rent Control Ordinance?`))

names(fannie_freddie_finances)
st_croix <- subset(fannie_freddie_finances_rc, `Property State` == "WI" &`Metropolitan Statistical Area`=="MINNEAPOLIS-ST. PAUL-BLOOMINGTON, MN-WI METROPOLITAN STATISTICAL AREA")
print(unique(st_croix$`Property City`))

ggplot(st_croix %>% filter(Year >2000), aes(x = Year, y = `Original Interest Rate`, color = `Property City`, group = `Property City`)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") + # e.g., policy approval
  labs(title = "LTV Ratios Over Time", y = "Loan-to-Value Ratio", x = "Year") +
  theme_minimal()

