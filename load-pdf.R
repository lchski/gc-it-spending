# https://www.ourcommons.ca/Committees/en/OGGO/StudyActivity?studyActivityId=10769410
# https://smallpdf.com/result#r=c49f0c682bf3d3bd9e245cda4a19b398&t=pdf-to-excel

library(readxl)

expenditures_201819_sheets <- "data/source/TreasuryBoardSecretariat-2-e-converted.xlsx"

expenditures_201819 <- tibble(sheet = 2:44) %>%
  mutate(raw_excel = map(sheet, ~ read_excel(expenditures_201819_sheets, .x)))

remove_header_row_from_raw_excel <- function(sheet, raw_excel) {
  if (nrow(raw_excel) > 27 & ! sheet %in% c(10, 16, 19)) { ## these added an extra header row
    return(
      raw_excel %>% slice(-1)
    )
  } else {
    return(
      raw_excel
    )
  }
}

remove_extra_rows_from_spend_data <- function(sheet, spend_data) {
  if (sheet %in% c(10, 16, 7, 19)) { ## first two have a bunch of extra rows for some reason; last two have misc comment row
    return(
      spend_data %>% slice(1:20)
    )
  } else {
    return(
      spend_data
    )
  }
}

remove_broken_columns_from_spend_data <- function(spend_data) {
  ## sheets with broken columns: c(2, 10, 18, 19, 24)
  
  good_columns <- c(
    "expenditure_category",
    "distributed_computing",
    "application_database_development_and_maintenance",
    "production_and_operations_computing",
    "telecommunications_data_and_voice",
    "it_security",
    "it_program_management",
    "total"
  )
  
  spend_data %>%
    select(good_columns)
}

zz <- expenditures_201819 %>%
  mutate(raw_excel = map2(sheet, raw_excel, remove_header_row_from_raw_excel)) %>%
  mutate(dept = map_chr(raw_excel, ~ .[[2,2]])) %>%
  select(-raw_excel) %>%
  mutate(spend_data = map(sheet, function(sheet_to_load) {
    if (sheet_to_load %in% c(3, 5, 7, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38)) { ## sheets with an extra row
      read_excel(expenditures_201819_sheets, sheet_to_load, skip = 8)
    } else {
      read_excel(expenditures_201819_sheets, sheet_to_load, skip = 7)
    }
  })) %>%
  mutate(spend_data = map2(sheet, spend_data, remove_extra_rows_from_spend_data)) %>%
  mutate(spend_data = map(spend_data, clean_names)) %>%
  mutate(spend_data = map(spend_data, remove_broken_columns_from_spend_data)) %>%
  mutate(spend_data = map(spend_data, ~ mutate_all(.x, as.character))) %>%
  unnest_wider(spend_data) %>%
  unnest(c(expenditure_category:total)) %>%
  mutate_at(vars(distributed_computing:total), ~ gsub("[^-0-9]", "", .x)) %>%
  mutate_at(vars(distributed_computing:total), as.integer) %>%
  mutate(expenditure_category = case_when(
    expenditure_category == "Allowances including EBP (20% of Salary)" ~ "Allowances incl EBP (20% of Salary)",
    TRUE ~ expenditure_category
  )) %>%
  mutate(expenditure_category_sub1 = case_when(
    expenditure_category %in% c(
      "Salary",
      "Allowances incl EBP (20% of Salary)",
      "Benefits paid by TBS (8.5% of Salary)",
      "Office Accommodation (13% of Salary)",
      "Training, Travel & Other HR Expenses",
      "Professional Services",
      "Cloud Services",
      "Other External Services"
    ) ~ expenditure_category,
    expenditure_category %in% c(
      "Software as a Service (SaaS)",
      "Platform as a Service (PaaS)",
      "Infrastructure as a Service (IaaS)"
    ) ~ "Cloud Services",
    TRUE ~ NA_character_
  )) %>%
  mutate(expenditure_category_sub2 = case_when(
    expenditure_category %in% c(
      "Software as a Service (SaaS)",
      "Platform as a Service (PaaS)",
      "Infrastructure as a Service (IaaS)"
    ) ~ expenditure_category,
    TRUE ~ NA_character_
  )) %>%
  mutate(expenditure_category = case_when(
    expenditure_category %in% c(
      "Software as a Service (SaaS)",
      "Platform as a Service (PaaS)",
      "Infrastructure as a Service (IaaS)"
    ) ~ "Cloud Services",
    TRUE ~ expenditure_category
  )) %>%
  mutate(expenditure_category = case_when(
    expenditure_category %in% c(
      "Salary",
      "Allowances incl EBP (20% of Salary)",
      "Benefits paid by TBS (8.5% of Salary)",
      "Office Accommodation (13% of Salary)",
      "Training, Travel & Other HR Expenses",
      "Professional Services",
      "Cloud Services",
      "Other External Services"
    ) ~ NA_character_,
    TRUE ~ expenditure_category
  )) %>%
  fill(expenditure_category) %>%
  mutate(is_subtotal_row = case_when(
    expenditure_category %in% c("Human Resources", "External Services") & is.na(expenditure_category_sub1) ~ TRUE,
    expenditure_category_sub1 == "Cloud Services" & is.na(expenditure_category_sub2) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(is_total_row = expenditure_category == "Total") %>%
  mutate(is_totaling_row = is_subtotal_row | is_total_row)




zz1 <- zz %>%
  filter(! is_totaling_row) %>%
  group_by(sheet, dept) %>%
  summarize_at(vars(distributed_computing:total), ~ sum(.x, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = distributed_computing:total, names_to = "program", values_to = "spend")

zz2 <- zz %>%
  filter(is_total_row) %>%
  select(sheet, dept, distributed_computing:total) %>%
  mutate_all(~ replace_na(.x, c(0))) %>%
  pivot_longer(cols = distributed_computing:total, names_to = "program", values_to = "spend")

zz1 %>%
  left_join(zz2, by = c("sheet", "dept", "program")) %>%
  mutate(same = spend.x == spend.y) %>%
  filter(! same)
