library(tidyverse)
library(lubridate)
library(janitor)

library(helpers)
source("lib/helpers.R")

# source: https://large-government-of-canada-it-projects.github.io/
projects <- read_csv("data/source/2019-gc-it-projects.csv", col_types = cols(
    deptAcronym = col_character(),
    shortcode = col_character(),
    uniqueId = col_character(),
    department = col_character(),
    description = col_character(),
    totalBudget = col_character(),
    estimatedCompletionDate = col_date(format = "%B %e, %Y"),
    rawProvidedDate = col_character(),
    originalDocumentOrder = col_double(),
    isOver10M = col_logical(),
    isOver100M = col_logical()
  )) %>%
  clean_names() %>%
  mutate(spend = as.integer(gsub("\\D", "", total_budget)))

# source: https://open.canada.ca/data/en/dataset/53753f06-8b28-42d7-89f7-04cd014323b0
contracts <- read_csv("data/source/tpsgc-pwgsc_co-ch_tous-all.csv") %>%
  clean_names() %>%
  remove_extra_columns()

gsins <- read_csv("data/source/tpsgc-pwgsc_nibs-gsin.csv") %>%
  clean_names() %>%
  remove_extra_columns()

classify_gsins <- function(gsins) {
  case_when(
    str_starts(gsins, "JX|JI|WL") ~ "services for goods",
    str_starts(gsins, "N") ~ "goods",
    str_starts(gsins, "D|U|R|A") ~ "services",
    TRUE ~ "TBD"
  )
}

## https://open.canada.ca/data/en/dataset/a35cf382-690c-4221-a971-cf0fd189a46f
expenditures_ftes_by_program <- read_csv("data/source/rbpo_rppo_en.csv")

it_service_spending_ftes_201819 <- expenditures_ftes_by_program %>%
  filter(fy_ef == 2018) %>%
  filter(core_responsibility == "Internal Services") %>%
  filter(str_detect(program_name, "^Information Technology")) %>%
  select(organization, organization_id, actual_spending, actual_ftes)

expenditures_201819 <- read_excel("data/source/2018-19 IT spending.xlsx") %>%
  pivot_longer(cols = `Distributed Computing`:`Total`, names_to = "it_services_group") %>%
  clean_names() %>%
  left_join(read_csv("data/source/dept_acronyms.csv")) %>%
  select(dept, department, infobase_organization, everything())

expenditures_201819 %>% write_csv("data/out/expenditures_201819.csv")




