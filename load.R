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
