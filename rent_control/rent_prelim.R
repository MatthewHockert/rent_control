library(tidyverse)
dewey_rental_micro <- read.csv('../sample_dewey.csv')
names(dewey_rental_micro)

nj_permits <- read.table(
  "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region/ne9311y.txt",
  skip = 1,
  stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)

folder_path <- "/Users/matthewhockert/Desktop/Personal Info/rent_control/downloads/Northeast_Region"

files <- list.files(folder_path, pattern = "y\\.txt$", full.names = TRUE)
files

nj_data_list <- list()

for (i in seq_along(files)) {
  print(files[i])
  df <- read.csv(files[i], skip = 1, stringsAsFactors = FALSE)
  nj_data_list[[i]] <- df
}

all_cols <- unique(unlist(lapply(nj_data_list, names)))

nj_data_list <- lapply(nj_data_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  for (col in missing_cols) df[[col]] <- NA
  df[all_cols] <- lapply(df[all_cols], as.character)
  df
})

nj_all <- bind_rows(nj_data_list)
nj_all <- filter(nj_all,Code =="34")

nj_all$Date <- as.Date(paste0(nj_all$Date, "01"), format = "%Y%m%d")

nj_all <- nj_all %>%
  mutate(Name = gsub("\\.+", "", Name),      # Remove repeated dots
         Name = gsub("#", "", Name),         # Remove hash symbols
         Name = str_trim(Name),              # Trim leading/trailing spaces
         Name = str_to_title(Name))          # Standardize to Title Case

nj_summary <- nj_all %>%
  mutate(Bldgs = as.numeric(Bldgs)) %>%
  group_by(Date, Name) %>%
  summarise(Bldgs = sum(Bldgs, na.rm = TRUE)) %>%
  ungroup()

nj_summary <- nj_summary %>%
  mutate(Date = as.Date(Date))

print(unique(nj_summary$Name))
# Plot
ggplot(nj_summary, aes(x = Date, y = Bldgs, group = Name, color = Name)) +
  geom_line() +
  labs(title = "New Jersey Monthly 1-Unit Permits", x = "Date", y = "Permits Issued")+
  theme(legend.position = "none")




