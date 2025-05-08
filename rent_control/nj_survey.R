library(dplyr)
library(lubridate)

nj_survey <-readxl::read_excel("../openai_nj_rent_control_survey.xlsx")
print(unique(nj_survey$`Units-in-Structure Ordinance Applies to`))
print(unique(nj_survey$`Rent Increase Limit`))
print(unique(nj_survey$`Rent Control Office/Board`))

nj_survey_dates <-readxl::read_excel("../NJ_Rent_Control_Survey_with_dates.xlsx")
# NJ_Rent_Control_Survey_with_dates.xlsx
names(nj_survey_dates)

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
    }
  ) %>%
  ungroup()

# Filter rows where the earliest ordinance date is after 2000-01-01
nj_survey_filtered <- nj_survey_dates %>%
  filter(earliest_ordinance_date > ymd("2000-01-01"))

nj_survey_filtered <- nj_survey_filtered %>%
  mutate(Municipality_Clean = str_to_lower(str_trim(Municipality)))
