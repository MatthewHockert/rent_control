library(tidyverse)
library(tidycensus)
library(data.table)
library(dplyr)
library(stringdist)
library(fuzzyjoin)
library(sf)
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


nj_1980_pop_cs <- read_csv("../nhgis0013_csv/nhgis0013_ds104_1980_cty_sub.csv")
nj_1980_pop_cs <- filter(nj_1980_pop_cs,STATEA == "34")
names(nj_1980_pop_cs)
nj_1980_pop_cs$population_1980_cs <- nj_1980_pop_cs$C7L001
nj_1980_pop_cs$population_1980_cs

nj_1980_pop_place <- read_csv("../nhgis0013_csv/nhgis0013_ds104_1980_place.csv")
nj_1980_pop_place <- filter(nj_1980_pop_place,STATEA == "34")
nj_1980_pop_place$population_1980_places <- nj_1980_pop_place$C7L001

nj_1990_pop_cs <- read_csv("../nhgis0013_csv/nhgis0013_ds120_1990_cty_sub.csv")
nj_1990_pop_cs <- filter(nj_1990_pop_cs,STATEA == "34")
names(nj_1990_pop_cs)
nj_1990_pop_cs$population_1990_cs <- nj_1990_pop_cs$ET1001
nj_1990_pop_cs$population_1990_cs

nj_1990_pop_place <- read_csv("../nhgis0013_csv/nhgis0013_ds120_1990_place.csv")
nj_1990_pop_place <- filter(nj_1990_pop_place,STATEA == "34")
nj_1990_pop_place$population_1980_places <- nj_1990_pop_place$ET1001
nj_1990_pop_place$population_1980_places

names(nj_1980_pop_cs)
names(nj_1980_pop_place)
names(nj_1990_pop_cs)
names(nj_1990_pop_place)


nj_1980_cs <- nj_1980_pop_cs %>%
  transmute(
    GISJOIN,
    COUNTY,
    AREANAME,
    STATEA,
    YEAR = 1980,
    geography_type = "county_subdivision",
    population = C7L001
  )

# 1980 Place
nj_1980_place <- nj_1980_pop_place %>%
  transmute(
    GISJOIN,
    COUNTY = COUNTYA,
    AREANAME,
    STATEA,
    YEAR = 1980,
    geography_type = "place",
    population = C7L001
  )

# 1990 County Subdivision
nj_1990_cs <- nj_1990_pop_cs %>%
  transmute(
    GISJOIN,
    COUNTY,
    AREANAME = CTY_SUB,  # No AREANAME in 1990 cs, use CTY_SUBA as proxy
    STATEA,
    YEAR = 1990,
    geography_type = "county_subdivision",
    population = ET1001
  )

# 1990 Place
nj_1990_place <- nj_1990_pop_place %>%
  transmute(
    GISJOIN,
    COUNTY= COUNTYA,
    AREANAME = PLACE,  # No AREANAME in 1990 place, use PLACEA
    STATEA,
    YEAR = 1990,
    geography_type = "place",
    population = ET1001
  )

# Bind all together
nj_pop_all <- bind_rows(nj_1980_cs, nj_1980_place, nj_1990_cs, nj_1990_place)

nj_pop_all <- nj_pop_all %>%
  mutate(
    Place_Name_Clean = str_to_lower(AREANAME),                                 # lowercase
    Place_Name_Clean = str_replace_all(Place_Name_Clean, "[^a-z0-9\\s-]", ""),   # remove non-alphanumeric except dash and space
    Place_Name_Clean = str_replace_all(Place_Name_Clean, "\\s+", " "),           # normalize whitespace
    Place_Name_Clean = str_trim(Place_Name_Clean),                               # trim leading/trailing space
    Place_Name_Clean = str_replace_all(Place_Name_Clean, "\\b(cdp)\\b", ""),  # drop suffixes
    Place_Name_Clean = str_replace(Place_Name_Clean, "(?<=\\bcity) city\\b", ""),
    Place_Name_Clean = str_replace_all(Place_Name_Clean, "\\s+", " "),           # re-trim space after removal
    Place_Name_Clean = str_trim(Place_Name_Clean)                                # final trim
  )

print(unique(nj_pop_all$Place_Name_Clean))

nj_pop_all_cleaned <- nj_pop_all %>%
  group_by(Place_Name_Clean, population, YEAR, STATEA) %>%
  mutate(
    keep = case_when(
      n() > 1 & any(geography_type == "county_subdivision") ~
        if_else(geography_type == "county_subdivision", TRUE, FALSE),
      TRUE ~ TRUE
    )
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-keep)


city_pop_names <- unique(city_population_matched$Place_Name_Clean)
city_pop_names

nj_pop_all2 <- filter(nj_pop_all_cleaned, Place_Name_Clean %in% city_pop_names)
nj_pop_all2 %>%
  group_by(YEAR) %>%
  summarise(n_unique_places = n_distinct(Place_Name_Clean),
            count = length(Place_Name_Clean))

nj_pop_all2 %>%
  group_by(YEAR, Place_Name_Clean) %>%
  summarise(n = n(), counties = n_distinct(COUNTY), .groups = "drop") %>%
  filter(n > 1 | counties > 1)%>%
  print(n=100)

nj_pop_all2 <- nj_pop_all2 %>%
  mutate(expand = case_when(
    YEAR == 1980 ~ 10,
    YEAR == 1990 ~ 10,
    TRUE ~ 1
  )) %>%
  uncount(weights = expand) %>%
  group_by(GISJOIN, YEAR) %>%
  mutate(Year = if (unique(YEAR) == 1980) 1980:1989 else if (unique(YEAR) == 1990) 1990:1999 else YEAR) %>%
  ungroup() 


nj_pop_all3 <- nj_pop_all2 %>%
  select(-GISJOIN) %>%
  rename(County_Name = COUNTY)%>%
  bind_rows(
    city_population_matched %>%
      transmute(
        Place_Name_Clean,
        County_Name,
        Year = as.numeric(Year),
        population
      )
  )
