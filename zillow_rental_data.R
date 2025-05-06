library(tidyverse)
zillow_rental_micro <- read.csv('/Users/matthewhockert/Downloads/Zillow properties listing information.csv')
names(zillow_rental_micro)

zip_zillow <- read_csv("Zip_zori_uc_sfrcondomfr_sm_month.csv")
zip_zillow_long <- zip_zillow %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Date", 
               values_to = "Rent") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Year = (format(Date, "%Y"))) 
# %>%
#   mutate(year_month = format(Date, "%Y-%m"))
  

zip_zillow_long <- zip_zillow_long %>%
  group_by(RegionName, year_month)%>%
  summarise(rent_avg = mean(Rent,na.rm=T),
            rent_med = median(Rent,na.rm=T),
            state=first(State),
            city = first(City),
            year = first(Year)) %>%
  filter(city %in% c("Saint Paul", "Minneapolis", "Boston", "Portland")) %>%
  filter(state != "OR")

# , 'Greensboro','Pittsburgh','Cincinnati','Buffalo' 

zip_zillow_long_mut <- zip_zillow_long %>%
  arrange(city, year) %>%
  group_by(city) %>%
  mutate(
    rent_i = rent_avg / first(rent_avg[year == "2021"]),
    rent_i_med = rent_med / first(rent_med[year == "2021"]),
    PercentChangeAvgRent = (rent_avg - lag(rent_avg)) / lag(rent_avg) * 100,
    PercentChangeMedRent = (rent_med - lag(rent_med)) / lag(rent_med) * 100
  )


ggplot(zip_zillow_long_mut %>% filter(year >2015), aes(x = year, y = PercentChangeAvgRent, color = as.factor(city), group = city)) +
  geom_line() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red") +   # Dashed line for 2023
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black") + # Dashed line for 2022
  labs(title = "Incident Counts Over Time",
       x = "Year",
       y = "Count of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

zip_zillow_long$treated <- ifelse(zip_zillow_long$city%in% c("Saint Paul","Minneapolis","Boston","Portland"),1,0)
zip_zillow_long$post <- ifelse(zip_zillow_long$year>=2021,1,0)

summary(lm(PercentChangeMedRent~post*treated+city+factor(year),zip_zillow_long)) 
summary(lm(PercentChangeMedRent ~ factor(year)*treated + city, data = zip_zillow_long))
summary(lm(rent_med ~ factor(year)*treated + city+factor(RegionName), data = zip_zillow_long))






