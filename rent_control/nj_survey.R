library(dplyr)
library(lubridate)
library(sf)

#### muni shapes ----
nj_muni_shape <- st_read("../NJ_Municipal_Boundaries_3424_2278012401268357461")
#nj_muni_shape$geometry <- st_centroid(nj_muni_shape$geometry)
nj_muni_shape <- select(nj_muni_shape,c("NAME","COUNTY","MUN_TYPE","SQ_MILES","geometry"))
#st_write(nj_muni_shape,"nj_muni_shape.shp",append=F)
nj_muni_shape <- nj_muni_shape %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(NAME)),
         County_Name = str_to_title(str_trim(COUNTY)))

nj_muni_df <- st_drop_geometry(nj_muni_shape)


reference_names <- nj_muni_df %>%
  distinct(Municipality_Clean, County_Name)

# con_stag_year should also have Place_Name_Clean and Year
con_names <- con_stag_year_pop %>%
  distinct(Place_Name_Clean,County_Name, Year)

# 2. Fuzzy match by name (but retain year from both sides)
# Step 1: Fuzzy match only by name
match_table <- stringdist_inner_join(
  con_names,                     # has Place_Name_Clean + Year
  reference_names,              # has Municipality_Clean + County_Name
  by = c("Place_Name_Clean" = "Municipality_Clean","County_Name"="County_Name"),
  method = "jw",
  max_dist = 0.2,
  distance_col = "distance"
) %>%
  rename(name_con = Place_Name_Clean, name_ref = Municipality_Clean)

# Step 2: If desired, filter best match per name-year
match_table_filtered <- match_table %>%
  group_by(name_con, Year) %>%
  slice_min(order_by = Place_Name_Clean.distance, n = 1) %>%
  ungroup()%>%
  rename("County_Name" = "County_Name.x")

nj_muni_df_improved <- nj_muni_df %>%
  left_join(
    match_table_filtered,
    by = c("Municipality_Clean" = "name_ref", "County_Name")
  )


years <- 1980:max(nj_muni_df_improved$Year, na.rm = TRUE)

municipality_grid <- nj_muni_df_improved %>%
  distinct(name_con, County_Name) %>%  # only valid pairs
  crossing(Year = years)               # repeat those pairs across years

nj_muni_df_filled <- municipality_grid %>%
  left_join(nj_muni_df_improved, by = c("name_con", "County_Name", "Year")) %>%
  arrange(name_con, County_Name, Year) %>%
  group_by(name_con, County_Name) %>%
  fill(NAME, COUNTY, MUN_TYPE, SQ_MILES, Municipality_Clean, .direction = "downup") %>%
  ungroup()

print(nj_muni_df_expanded_filled,n=100)
names(nj_muni_df_improved)

permits_city_size_anti_join <- anti_join(
  con_stag_year_pop,
  nj_muni_df_filled %>%
    rename(
      Place_Name_Clean = name_con
    ),
  by = c("Place_Name_Clean", "County_Name", "Year")
)

min(nj_muni_df_improved$Year,na.rm=T)
min(con_stag_year_pop$Year,na.rm=T)



#### Survey Continuous treatment ----
nj_survey <-readxl::read_excel("../openai_nj_rent_control_survey.xlsx")
print(unique(nj_survey$`Units-in-Structure Ordinance Applies to`))
print(unique(nj_survey$`Rent Increase Limit`))
print(unique(nj_survey$`Rent Control Office/Board`))


rent_control_intensity <- read_excel("../rent_control_scored_output.xlsx")

rent_control_intensity <- rent_control_intensity %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))

rent_control_intensity <- merge(
  rent_control_intensity,
  nj_crosswalk,
  by.x = "Municipality_Clean",
  by.y = "rent_control_name",
  all.x = TRUE
)
names(rent_control_intensity)
rent_control_intensity <- rent_control_intensity %>% select(-c("Units-in-Structure Ordinance Applies to","Rent Increase Limit","Exceptions"))

head(rent_control_intensity)

#### Survey dates ----
nj_survey_dates <-readxl::read_excel("../NJ_Rent_Control_Survey_with_date2.xlsx")
sum(is.na(nj_survey_dates$ordinance_date_1))
# NJ_Rent_Control_Survey_with_dates.xlsx
names(nj_survey_dates)
print(unique(nj_survey_dates$`Rent Control Office/Board`))
# Convert ordinance date columns to Date type
date_cols <- grep("^ordinance_date_", names(nj_survey_dates), value = TRUE)
nj_survey_dates[date_cols] <- lapply(nj_survey_dates[date_cols], ymd)

nj_survey_dates <- nj_survey_dates %>%
  rowwise() %>%
  mutate(
    earliest_ordinance_date = if (all(is.na(c_across(all_of(date_cols))))) {
      NA_Date_
    } else {
      min(c_across(all_of(date_cols)), na.rm = TRUE)
    },
    latest_ordinance_date = if (all(is.na(c_across(all_of(date_cols))))) {
      NA_Date_
    } else {
      max(c_across(all_of(date_cols)), na.rm = TRUE)
    },
    latest_pre2022_ordinance_date = {
      valid_dates <- c_across(all_of(date_cols))
      valid_pre2022 <- valid_dates[!is.na(valid_dates) & valid_dates < as.Date("2022-01-01")]
      if (length(valid_pre2022) == 0) NA_Date_ else max(valid_pre2022)
    }
  ) %>%
  ungroup()

#### nj new scraper survey dates----

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Load the data
nj <- read_excel("../rent_control_raw_dates_by_city.xlsx")

# Identify the ordinance date columns
date_cols <- c(
  'RentControl','RentIncreaseLimit','Exemptions','UnitsStructure','VacancyIncrease',
  'VacancyControl','HardshipIncreases','CapitalImprovements','AdministrativeProcedures',
  'EvictionControls','RegistrationRequirements','FeeSchedules','ExpirationOrSunset','Definitions'
)

# Function to parse semicolon-separated dates into Date objects
parse_dates <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(as.Date(NA))
  dates <- str_split(x, ";")[[1]]
  dates <- suppressWarnings(ymd(dates))
  dates[!is.na(dates)]
}

# Apply date parsing to each column
for (col in date_cols) {
  nj[[paste0(col, "_parsed")]] <- lapply(nj[[col]], parse_dates)
}

# Get earliest, latest, and latest pre-2022 date across all parsed fields
nj <- nj %>%
  rowwise() %>%
  mutate(
    all_dates = list(unlist(c_across(ends_with("_parsed")))),
    earliest_ordinance_date = if (length(all_dates) == 0) NA_Date_ else min(all_dates, na.rm = TRUE),
    latest_ordinance_date = if (length(all_dates) == 0) NA_Date_ else max(all_dates, na.rm = TRUE),
    latest_pre2022_ordinance_date = {
      pre2022 <- all_dates[all_dates < as.Date("2022-01-01")]
      if (length(pre2022) == 0) NA_Date_ else max(pre2022)
    }
  ) %>%
  ungroup()

# Optional: Drop intermediate parsed columns
nj <- nj %>% select(-ends_with("_parsed"), -all_dates)

