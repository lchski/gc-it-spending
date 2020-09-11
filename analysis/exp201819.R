## gut check vs PDF
expenditures_201819 %>%
  filter(expenditure_category == "Total", it_services_group == "Total") %>%
  select(department, value) %>%
  mutate(value = scales::dollar(value))

expenditures_201819

it_service_spending_ftes_201819

## get a (very rough) sense of "shadow IT" percentage:
## ratio is `IT services group spending / total departmental IT spending`
## where IT services group is the InfoBase "Internal Services" IT group (could also include IM, if we altered earlier calls)
expenditures_201819 %>%
  filter(expenditure_category == "Total", it_services_group == "Total") %>%
  select(infobase_organization, value) %>%
  left_join(it_service_spending_ftes, by = c("infobase_organization" = "organization")) %>%
  filter(! is.na(infobase_organization)) %>%
  mutate(ratio = actual_spending / value)
