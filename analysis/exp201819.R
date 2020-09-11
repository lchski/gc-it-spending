## gut check vs PDF
expenditures_201819 %>%
  filter(expenditure_category == "Total", it_services_group == "Total") %>%
  select(department, value) %>%
  mutate(value = scales::dollar(value))

expenditures_201819
