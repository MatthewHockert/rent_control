library(tidyverse)
library(tidycensus)
library(data.table)
library(dplyr)
library(stringdist)
library(fuzzyjoin)
nj_counties <- st_read('../cb_2024_us_county_500k')
nj_counties <- filter(nj_counties,STATEFP=="34")
msa <- st_read("../cb_2024_us_metdiv_500k")

census_api_key("03e262234bdb241c878de68897395da1a35a6843", install = FALSE)

state_fips <- "34"

# Pull decennial total population at the city level
pull_city_and_township_population <- function(year) {
  var <- if (year == 2020) "P1_001N" else "P001001"
  get_geom <- year == 2020
  
  place_data <- get_decennial(
    geography = "place",
    variables = var,
    year = year,
    state = state_fips,
    geometry = get_geom
  ) %>%
    select(GEOID, NAME, value) %>%
    mutate(geo_type = "place")
  
  csub_data <- get_decennial(
    geography = "county subdivision",
    variables = var,
    year = year,
    state = state_fips,
    geometry = get_geom
  ) %>%
    select(GEOID, NAME, value) %>%
    mutate(geo_type = "county_subdivision")
  
  combined <- bind_rows(place_data, csub_data) %>%
    rename(population = value) %>%
    mutate(decade = year)
  
  return(combined)
}
# Pull once per decade
pop_2000 <- pull_city_and_township_population(2000)
pop_2010 <- pull_city_and_township_population(2010)
pop_2020 <- pull_city_and_township_population(2020)

# Repeat each city's row 10 times, then assign the year values
pop_2000 <- pop_2000 %>%
  uncount(weights = 10) %>%
  group_by(GEOID) %>%
  mutate(Year = 2000:2009) %>%
  ungroup()

pop_2010 <- pop_2010 %>%
  uncount(weights = 10) %>%
  group_by(GEOID) %>%
  mutate(Year = 2010:2019) %>%
  ungroup()

pop_2020 <- pop_2020 %>%
  uncount(weights = 6) %>%
  group_by(GEOID) %>%
  mutate(Year = 2020:2025) %>%
  ungroup()

# Combine all years
city_population_by_year <- bind_rows(pop_2000, pop_2010, pop_2020) %>%
  mutate(NAME = NAME %>%
           str_remove(",\\s*[^,]+ County") %>%
           str_remove(",\\s*New Jersey") %>%
           str_remove(regex("\\s*cdp$", ignore_case = TRUE)) %>%
           str_to_lower() %>%
           str_trim())

city_population_by_year <- city_population_by_year %>%
  arrange(NAME,geo_type, Year)

# city_population_by_year <- city_population_by_year %>%
#   mutate(NAME = str_remove(NAME, ", [^,]+ County"),
#          NAME = str_remove(NAME, ", New Jersey"),
#          NAME = str_replace(NAME, " cdp", ""),
#          NAME = str_to_lower(str_trim(NAME))
#   )

city_population_by_year<-st_as_sf(city_population_by_year)
sum(st_is_empty(city_population_by_year))
city_population_by_year %>%
  filter(st_is_empty(geometry))


city_population_by_year <- city_population_by_year %>%
  group_by(NAME, Year) %>%
  arrange(desc(geo_type == "place")) %>%  # prioritize "place"
  slice(1) %>%  # keep the first one (i.e., "place" if available)
  ungroup()


city_population_by_year_naming <- city_population_by_year %>%
  mutate(
    Place_Name_Clean = case_when(
      tolower(NAME) %in% c("princeton township", "princeton borough") ~ "princeton",
      tolower(NAME) %in% c("west paterson borough") ~ "woodland park borough",
      tolower(NAME) %in% c("pahaquarry township") ~ "hardwick township",
      tolower(NAME) %in% c("dover township")& NAME == "Ocean" ~ "toms river township",
      tolower(NAME) %in% c("south belmar borough") ~ "lake como borough",
      tolower(NAME) %in% c("pine valley borough") ~ "pine hill borough",
      tolower(NAME) %in% c("toms river") & NAME == "Ocean" ~ "toms river township",
      tolower(NAME) %in% c("washington township") & NAME == "Mercer" ~ "robbinsville township",
      TRUE ~ NAME
    )
  )%>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(NAME= first(NAME),
              across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

geometry_reference <- city_population_by_year_naming %>%
  filter(Year >= 2020, !st_is_empty(geometry)) %>%
  group_by(Place_Name_Clean) %>%
  slice_max(Year) %>%
  ungroup() %>%
  select(Place_Name_Clean, geometry)

geom_lookup <- st_geometry(geometry_reference)
names(geom_lookup) <- geometry_reference$Place_Name_Clean

city_population_by_year_filled <- city_population_by_year_naming
st_geometry(city_population_by_year_filled) <- mapply(
  function(current_geom, name) {
    if (st_is_empty(current_geom)) {
      geom_lookup[[name]]
    } else {
      current_geom
    }
  },
  st_geometry(city_population_by_year_naming),
  city_population_by_year_naming$Place_Name_Clean,
  SIMPLIFY = FALSE
) %>% st_sfc(crs = st_crs(city_population_by_year_naming))

city_population_by_year_filled <- st_centroid(city_population_by_year_filled)

nrow(city_population_by_year_filled)
city_population_by_year_filled <- st_intersection(city_population_by_year_filled,nj_counties)
# nrow(test)
city_population_by_year_filled2 <- st_drop_geometry(city_population_by_year_filled)
head(city_population_by_year_filled)
beep()



head(city_population_by_year_filled2)
city_population_by_year_filled2 <- city_population_by_year_filled2%>%
  rename(County_Name=`NAME.1`)

reference_names <- city_population_by_year_filled2 %>%
  filter(decade == 2020) %>%
  select(Place_Name_Clean) %>%
  distinct()

match_table <- stringdist_inner_join(
  con_stag_year %>% distinct(Place_Name_Clean),
  reference_names,
  by = "Place_Name_Clean",
  method = "jw",
  max_dist = 0.2,
  distance_col = "distance"
) %>%
  rename(name_con = Place_Name_Clean.x, name_pop = Place_Name_Clean.y) %>%
  arrange(name_con, distance)

best_matches <- match_table %>%
  group_by(name_con) %>%
  slice_min(order_by = distance, n = 1) %>%
  ungroup()



city_population_by_year_filled2$Year <- as.character(city_population_by_year_filled2$Year)

city_population_matched <- best_matches %>%
  rename(Place_Name_Clean = name_con) %>%  # rename for downstream merge
  left_join(city_population_by_year_filled2, by = c("name_pop" = "Place_Name_Clean"),relationship = "many-to-many")

#,"Year"="Year"
#,relationship = "many-to-many"


