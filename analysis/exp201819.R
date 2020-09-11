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
  filter(! is.na(infobase_organization)) %>%
  group_by(infobase_organization) %>%
  summarize(spend = sum(value)) %>%
  left_join(it_service_spending_ftes %>% select(-organization_id), by = c("infobase_organization" = "organization")) %>%
  mutate(shadow_it_pct = actual_spending / spend) %>%
  mutate_at(vars(contains("spend")), scales::dollar) %>%
  mutate_at(vars(contains("pct")), ~ round(.x, 2)) %>%
  write_csv("data/out/shadow_it.csv")





## get a sense of how departments prioritize in-house vs external talent
department_total_spend <- expenditures_201819 %>%
  group_by(department) %>%
  summarize(spend = sum(value))

department_hr_spend <- expenditures_201819 %>%
  filter(expenditure_category == "Human Resources") %>%
  group_by(department) %>%
  summarize(hr_spend = sum(value))

department_consultant_spend <- expenditures_201819 %>%
  filter(expenditure_category == "Professional Services") %>%
  group_by(department) %>%
  summarize(consultant_spend = sum(value))

dept_talent_approach <- department_total_spend %>%
  left_join(department_hr_spend) %>%
  left_join(department_consultant_spend) %>%
  mutate(
    hr_pct = hr_spend / spend,
    consultant_pct = consultant_spend / spend,
    consultant_to_hr_pct = consultant_spend / hr_spend
  ) %>%
  mutate_at(vars(contains("pct")), ~ round(.x, 2))

dept_talent_approach %>%
  mutate_at(vars(contains("spend")), scales::dollar) %>%
  write_csv("data/out/dept_talent_approach.csv")






