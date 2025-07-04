library(tidyverse)
library(tidycensus)
library(data.table)
library(dplyr)
library(stringdist)
library(fuzzyjoin)
library(sf)
library(readxl)
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

nrow(city_population_by_year)
city_population_by_year_naming <- city_population_by_year %>%
  mutate(
    Place_Name_Clean = case_when(
      tolower(NAME) %in% c("princeton township", "princeton borough") ~ "princeton",
      tolower(NAME) %in% c("west paterson borough") ~ "woodland park borough",
      tolower(NAME) %in% c("pahaquarry township") ~ "hardwick township",
      tolower(NAME) %in% c("dover township") ~ "toms river township",
      tolower(NAME) %in% c("south belmar borough") ~ "lake como borough",
      tolower(NAME) %in% c("pine valley borough") ~ "pine hill borough",
      tolower(NAME) %in% c("toms river") ~ "toms river township",
      #tolower(NAME) %in% c("washington township") & NAME == "Mercer" ~ "robbinsville township",
      tolower(NAME) %in% c("verona borough township") ~ "verona township",
      tolower(NAME) %in% c("caldwell borough township") ~ "caldwell borough",
      tolower(NAME) %in% c("glen ridge borough township") ~ "glen ridge borough",
      tolower(NAME) %in% c("essex fells township") ~ "essex fells borough",
      tolower(NAME) %in% c("north caldwell township") ~ "north caldwell borough",
      tolower(NAME) %in% c("orange township", "orange township city") ~ "city of orange township",
      tolower(NAME) %in% c("carney's point township") ~ "carneys point township",
      tolower(NAME) %in% c("hawthorn borough") ~ "hawthorne borough",
      TRUE ~ NAME
    )
  )%>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(NAME= first(NAME),
              across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

nrow(city_population_by_year_naming)
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

city_population_by_year_filled_centroid <- st_centroid(city_population_by_year_filled)

nrow(city_population_by_year_filled)
city_population_by_year_filled <- st_intersection(city_population_by_year_filled_centroid,nj_counties)
nrow(city_population_by_year_filled)


no_merge<-anti_join(
  st_drop_geometry(city_population_by_year_filled_centroid), 
  st_drop_geometry(city_population_by_year_filled),
  by = ("Place_Name_Clean")
)

anti_join(
  st_drop_geometry(no_merge), 
  st_drop_geometry(con_stag_year),
  by = ("Place_Name_Clean")
)

city_population_by_year_filled2 <- st_drop_geometry(city_population_by_year_filled)
#head(city_population_by_year_filled)
beep()
city_population_by_year_filled2$County_Name <- city_population_by_year_filled2$NAME.1

city_population_by_year_filled2 <- city_population_by_year_filled2 %>%
  mutate(
    Place_Name_Clean = case_when(
      str_to_lower(NAME) %in% c("princeton township", "princeton borough") ~ "princeton",
      str_to_lower(NAME) == "west paterson borough" ~ "woodland park borough",
      str_to_lower(NAME) == "pahaquarry township" ~ "hardwick township",
      str_to_lower(NAME) == "dover township" & County_Name == "Ocean" ~ "toms river township",
      str_to_lower(NAME) == "toms river" & County_Name == "Ocean" ~ "toms river township",
      str_to_lower(NAME) == "washington township" & County_Name == "Mercer" ~ "robbinsville township",
      str_to_lower(NAME) == "south belmar borough" ~ "lake como borough",
      str_to_lower(NAME) == "pine valley borough" ~ "pine hill borough",
      str_to_lower(NAME) == "verona borough township" ~ "verona township",
      str_to_lower(NAME) == "caldwell borough township" ~ "caldwell borough",
      str_to_lower(NAME) == "glen ridge borough township" ~ "glen ridge borough",
      str_to_lower(NAME) == "essex fells township" ~ "essex fells borough",
      str_to_lower(NAME) == "north caldwell township" ~ "north caldwell borough",
      str_to_lower(NAME) %in% c("orange township", "orange township city") ~ "city of orange township",
      str_to_lower(NAME) == "carney's point township" ~ "carneys point township",
      str_to_lower(NAME) == "hawthorn borough" ~ "hawthorne borough",
      TRUE ~ str_to_lower(NAME)
    )
  )

nrow(city_population_by_year_filled2)

# 1. Prepare inputs with both name and year
reference_names <- city_population_by_year_filled2 %>%
  distinct(Place_Name_Clean, Year)

# con_stag_year should also have Place_Name_Clean and Year
con_names <- con_stag_year %>%
  distinct(Place_Name_Clean, Year)

# 2. Fuzzy match by name (but retain year from both sides)
match_table <- stringdist_inner_join(
  con_names,
  reference_names,
  by = c("Place_Name_Clean","Year"),
  method = "jw",
  max_dist = 0.2,
  distance_col = "distance"
) %>%
  rename(name_con = Place_Name_Clean.x,
         name_pop = Place_Name_Clean.y,
         Year = Year.x) %>%  # retain year from con_stag
  arrange(name_con, Year, distance)

# 3. Choose best match per place-year
best_matches <- match_table %>%
  group_by(name_con, Year) %>%
  slice_min(order_by = distance, n = 1) %>%
  ungroup()

city_population_by_year_filled2$Year <- as.numeric(city_population_by_year_filled2$Year)

nrow(city_population_by_year_filled2)
# 4. Join matched names + year to full population data
city_population_matched <- best_matches %>%
  left_join(city_population_by_year_filled2, by = c("name_pop" = "Place_Name_Clean", "Year"))

nrow(city_population_matched)
#,"Year"="Year"
#,relationship = "many-to-many"

#######
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

nrow(nj_pop_all_cleaned)
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

nj_pop_all_cleaned <- nj_pop_all_cleaned %>%
  mutate(expand = case_when(
    YEAR == 1980 ~ 10,
    YEAR == 1990 ~ 10,
    TRUE ~ 1
  )) %>%
  uncount(weights = expand) %>%
  group_by(GISJOIN, YEAR) %>%
  mutate(Year = if (unique(YEAR) == 1980) 1980:1989 else if (unique(YEAR) == 1990) 1990:1999 else YEAR) %>%
  ungroup() 


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
nrow(nj_pop_all3)

nj_pop_all3 %>%
  group_by(Place_Name_Clean) %>%
  summarise(num_years = n_distinct(Year)) %>%
  arrange(num_years)%>%
  print(n=1000)


##### pull NJ population data ----
nj_1980 <- read_excel("../populations/nj_population_1980_89.xlsx")
nj_1990 <- read_excel("../populations/inter9099.xlsx")
nj_2000 <- read_excel("../populations/mcdest2009-2.xlsx")
nj_2010 <- read_excel("../populations/sub-mcd-est2020int-pop-34.xlsx")
nj_2020 <- read_excel("../populations/SUB-MCD-EST2024-POP-34.xlsx")

nj_1980_long <- nj_1980 %>%
  pivot_longer(
    cols = -c(Municipality, County),
    names_to = "Year",
    values_to = "population"
  )

nj_1990_long <- nj_1990 %>%
  pivot_longer(cols = -c(Municipality, County), names_to = "Year", values_to = "population")

nj_2000_long <- nj_2000 %>%
  pivot_longer(cols = -c(Municipality, County), names_to = "Year", values_to = "population")

nj_2010_long <- nj_2010 %>%
  pivot_longer(cols = -c(Municipality, County), names_to = "Year", values_to = "population")

nj_2020_long <- nj_2020 %>%
  pivot_longer(cols = -c(Municipality, County), names_to = "Year", values_to = "population")


nj_population <- rbind(nj_1980_long, nj_1990_long, nj_2000_long, nj_2010_long, nj_2020_long)

nj_population <- nj_population %>%
  arrange(Municipality,year)

nj_population <- nj_population %>%
  mutate(
    Place_Name_Clean = str_to_lower(Municipality),
    Place_Name_Clean = str_replace(Place_Name_Clean, regex("(city|borough|township|village|town) \\1$", ignore_case = TRUE), "\\1")
  )
print(unique(nj_population$County))
nj_population$County <- gsub("(?i)\\s*County\\b", "", nj_population$County, perl = TRUE)
nj_population$County_Name <- trimws(nj_population$County)


nj_population_updated <- nj_population %>%
  mutate(
    Place_Name_Clean = case_when(
      tolower(Place_Name_Clean) %in% c("princeton township", "princeton borough") ~ "princeton",
      tolower(Place_Name_Clean) %in% c("west paterson borough") ~ "woodland park borough",
      tolower(Place_Name_Clean) %in% c("pahaquarry township") ~ "hardwick township",
      tolower(Place_Name_Clean) %in% c("dover township") & County_Name == "Ocean" ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("south belmar borough") ~ "lake como borough",
      tolower(Place_Name_Clean) %in% c("pine valley borough") ~ "pine hill borough",
      tolower(Place_Name_Clean) %in% c("toms river town") ~ "toms river township",
      tolower(Place_Name_Clean) %in% c("washington township") & County_Name == "Mercer" ~ "robbinsville township",
      tolower(Place_Name_Clean) %in% c("peapack and gladstone boro") ~ "peapack and gladstone borough",
      tolower(Place_Name_Clean) %in% c("orange") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("south orange village") ~ "south orange village township",
      tolower(Place_Name_Clean) %in% c("west orange town") ~ "west orange township",
      tolower(Place_Name_Clean) %in% c("orange township city") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("orange township") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("pahaquarry township (n)") ~ "hardwick township",
      tolower(Place_Name_Clean) %in% c("walpack township (n)") ~ "walpack township",
      tolower(Place_Name_Clean) %in% c("carney's point township") ~ "carneys point township",
      tolower(Place_Name_Clean) %in% c("hawthorn borough") ~ "hawthorne borough",
      tolower(Place_Name_Clean) %in% c("verona borough township") ~ "verona township",
      tolower(Place_Name_Clean) %in% c("glen ridge borough township") ~ "glen ridge borough",
      tolower(Place_Name_Clean) %in% c("orange city township") ~ "city of orange township",
      tolower(Place_Name_Clean) %in% c("glen ridge township") ~ "glen ridge borough",
      tolower(Place_Name_Clean) %in% c("north caldwell township") ~ "north caldwell borough",
      tolower(Place_Name_Clean) %in% c("caldwell borough township") ~ "caldwell borough",
      tolower(Place_Name_Clean) %in% c("passaic township") ~ "long hill township",
      tolower(Place_Name_Clean) %in% c("essex fells township") ~ "essex fells borough",
      tolower(Place_Name_Clean) %in% c("westhampton township") ~ "westampton township",
      tolower(Place_Name_Clean) %in% c("belleville town") ~ "belleville township",
      tolower(Place_Name_Clean) %in% c("bloomfield town") ~ "bloomfield township",
      tolower(Place_Name_Clean) %in% c("nutley town") ~ "nutley township",
      tolower(Place_Name_Clean) %in% c("verona borough") ~ "verona township",
      tolower(Place_Name_Clean) %in% c("west caldwell borough") ~ "west caldwell township",
      tolower(Place_Name_Clean) %in% c("matawan township") ~ "aberdeen township",
      tolower(Place_Name_Clean) %in% c("irvington town") ~ "irvington township",
      tolower(Place_Name_Clean) %in% c("montclair town") ~ "montclair township",
      tolower(Place_Name_Clean) %in% c("absecon city") ~ "absecon",
      tolower(Place_Name_Clean) %in% c("asbury park city") ~ "asbury park",
      tolower(Place_Name_Clean) %in% c("bayonne city") ~ "bayonne",
      tolower(Place_Name_Clean) %in% c("beverly city") ~ "beverly",
      tolower(Place_Name_Clean) %in% c("bordentown city") ~ "bordentown",
      tolower(Place_Name_Clean) %in% c("bridgeton city") ~ "bridgeton",
      tolower(Place_Name_Clean) %in% c("brigantine city") ~ "brigantine",
      tolower(Place_Name_Clean) %in% c("burlington city") ~ "burlington",
      tolower(Place_Name_Clean) %in% c("camden city") ~ "camden",
      TRUE ~ Place_Name_Clean
    )
  ) %>%
  group_by(Place_Name_Clean, Year) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
            County_Name = first(County_Name),
            .groups = "drop")
nj_population_updated$Place_Name_Clean <- nj_population_updated$Place_Name_Clean %>%
  tolower() %>%
  gsub("^asbury park city$", "asbury park", .) %>%
  gsub("^bayonne city$", "bayonne", .) %>%
  gsub("^beverly city$", "beverly", .) %>%
  gsub("^bordentown city$", "bordentown", .) %>%
  gsub("^bridgeton city$", "bridgeton", .) %>%
  gsub("^brigantine city$", "brigantine", .) %>%
  gsub("^burlington city$", "burlington", .) %>%
  gsub("^camden city$", "camden", .) %>%
  gsub("^cape may city$", "cape may", .) %>%
  gsub("^clifton city$", "clifton", .) %>%
  gsub("^east orange city$", "east orange", .) %>%
  gsub("^elizabeth city$", "elizabeth", .) %>%
  gsub("^englewood city$", "englewood", .) %>%
  gsub("^estell manor city$", "estell manor", .) %>%
  gsub("^fairfield city$", "fairfield", .) %>%
  gsub("^garfield city$", "garfield", .) %>%
  gsub("^hackensack city$", "hackensack", .) %>%
  gsub("^hoboken city$", "hoboken", .) %>%
  gsub("^lambertville city$", "lambertville", .) %>%
  gsub("^linden city$", "linden", .) %>%
  gsub("^linwood city$", "linwood", .) %>%
  gsub("^long branch city$", "long branch", .) %>%
  gsub("^millville city$", "millville", .) %>%
  gsub("^new brunswick city$", "new brunswick", .) %>%
  gsub("^newark city$", "newark", .) %>%
  gsub("^north wildwood city$", "north wildwood", .) %>%
  gsub("^northfield city$", "northfield", .) %>%
  gsub("^passaic city$", "passaic", .) %>%
  gsub("^paterson city$", "paterson", .) %>%
  gsub("^perth amboy city$", "perth amboy", .) %>%
  gsub("^plainfield city$", "plainfield", .) %>%
  gsub("^pleasantville city$", "pleasantville", .) %>%
  gsub("^port republic city$", "port republic", .) %>%
  gsub("^rahway city$", "rahway", .) %>%
  gsub("^salem city$", "salem", .) %>%
  gsub("^somers point city$", "somers point", .) %>%
  gsub("^south amboy city$", "south amboy", .) %>%
  gsub("^summit city$", "summit", .) %>%
  gsub("^trenton city$", "trenton", .) %>%
  gsub("^vineland city$", "vineland", .) %>%
  gsub("^wildwood city$", "wildwood", .) %>%
  gsub("^woodbury city$", "woodbury", .)

print(unique(nj_population$County))

nj_population_updated_cleaned <- nj_population_updated %>%
  filter(!Place_Name_Clean %in% unique(nj_population_updated$County_Name))


nj_counties <- tolower(c(
  "Atlantic", "Bergen", "Burlington", "Camden", "Cape May", "Cumberland",
  "Essex", "Gloucester", "Hudson", "Hunterdon", "Mercer", "Middlesex",
  "Monmouth", "Morris", "Ocean", "Passaic", "Salem", "Somerset",
  "Sussex", "Union", "Warren"
))
nj_counties <- paste(nj_counties, "county")


nj_population_updated_cleaned <- nj_population_updated_cleaned %>%
  filter(!Place_Name_Clean %in% nj_counties)

nj_population_updated_cleaned <- nj_population_updated_cleaned %>%
  mutate(Place_Name_Clean = case_when(
    Place_Name_Clean == "hackettstown" ~ "hackettstown town",
    Place_Name_Clean == "morristown" ~ "morristown town",
    TRUE ~ Place_Name_Clean
  ))

nj_population_updated_cleaned <- nj_population_updated_cleaned %>%
  mutate(County_Name = str_replace_all(County_Name, "'$", ""))%>%
  group_by(Place_Name_Clean) %>%
  tidyr::fill(County_Name, .direction = "updown") %>%
  ungroup()



