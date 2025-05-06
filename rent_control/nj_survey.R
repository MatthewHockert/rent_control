nj_survey <-readxl::read_excel("../openai_nj_rent_control_survey.xlsx")
print(unique(nj_survey$`Units-in-Structure Ordinance Applies to`))
print(unique(nj_survey$`Rent Increase Limit`))
