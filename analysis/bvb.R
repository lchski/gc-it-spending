source("load.R")

software_gsin_descs_excl_maintenance <- contracts %>%
  count_group(gsin, gsin_description_en) %>%
  ungroup() %>%
  filter(str_detect(gsin_description_en, regex("software", ignore_case = TRUE))) %>%
  filter(! str_detect(gsin_description_en, regex("maintenance", ignore_case = TRUE))) %>%
  pull(gsin_description_en)

software_informatics_gsin_descs_excl_maintenance <- contracts %>%
  count_group(gsin, gsin_description_en) %>%
  ungroup() %>%
  filter(str_detect(gsin_description_en, regex("software|informatics", ignore_case = TRUE))) %>%
  filter(! str_detect(gsin_description_en, regex("maintenance", ignore_case = TRUE))) %>%
  pull(gsin_description_en)

informatics_gsin_descs_excl_maintenance <- contracts %>%
  count_group(gsin, gsin_description_en) %>%
  ungroup() %>%
  filter(str_detect(gsin_description_en, regex("informatics", ignore_case = TRUE))) %>%
  filter(! str_detect(gsin_description_en, regex("maintenance", ignore_case = TRUE))) %>%
  pull(gsin_description_en)

scope_contracts_by_gsins <- function(contracts, gsins) {
  software_contract_numbers_with_amendment_000 <- contracts %>%
    filter(gsin_description_en %in% gsins) %>%
    select(contract_number, amendment_number) %>%
    filter(amendment_number == "000") %>%
    pull(contract_number)
  
  software_contracts_in_scope <- contracts %>%
    filter(contract_number %in% software_contract_numbers_with_amendment_000) %>%
    mutate(gsin_type = classify_gsins(gsin)) %>%
    mutate(contract_length = time_length(interval(award_date, expiry_date), "months")) %>%
    mutate(contract_length_bin = cut(
      contract_length,
      breaks = c(-Inf, 0, 6, 12, Inf),
      labels = c("negative length", "<6 mos", "6 mos to 1 yr", ">1 yr")
    )) %>%
    mutate(contract_value_bin = cut(
      contract_value,
      breaks = c(-Inf, 0, 1000000, 10000000, Inf),
      labels = c("<$0", "<$1M", "$1M to $10M", ">$10M")
    ))
  
  return(software_contracts_in_scope)
}

software_contracts <- contracts %>%
  scope_contracts_by_gsins(software_gsin_descs_excl_maintenance)

software_informatics_contracts <- contracts %>%
  scope_contracts_by_gsins(software_informatics_gsin_descs_excl_maintenance)

informatics_contracts <- contracts %>%
  scope_contracts_by_gsins(informatics_gsin_descs_excl_maintenance)



# double check that summing the `contract_value` column comes out to the same count as `total_contract_value`
# should be 0 rows returned if we're right
sanity_check_contract_value_total_contract_value <- function(contracts_to_check) {
  contracts_to_check %>%
    group_by(contract_number) %>%
    summarize(
      total = sum(contract_value),
      min_total = min(total_contract_value),
      max_total = max(total_contract_value)
    ) %>%
    pivot_longer(cols = c(total:max_total)) %>%
    group_by(contract_number) %>% 
    summarize(
      n = n(),
      n_distinct = n_distinct(value)
    ) %>%
    filter(n_distinct > 1)
}

software_contracts %>% sanity_check_contract_value_total_contract_value
software_informatics_contracts %>% sanity_check_contract_value_total_contract_value
informatics_contracts %>% sanity_check_contract_value_total_contract_value


## basic description (count, %, $ total) for grouped contracts
describe_contracts_by <- function(contracts_to_describe, ...) {
  contracts_to_describe %>%
    group_by(...) %>%
    summarize(
      n = n(),
      value = sum(contract_value)
    ) %>%
    mutate(
      n_prop = n / sum(n),
      value_prop = value / sum(value)
    ) %>%
    select(..., n, n_prop, value, value_prop)
}

### contract_length_bin
software_contracts %>%
  describe_contracts_by(contract_length_bin)

software_informatics_contracts %>%
  describe_contracts_by(contract_length_bin)

informatics_contracts %>%
  describe_contracts_by(contract_length_bin)


### contract_value_bin
software_contracts %>%
  describe_contracts_by(contract_value_bin)

software_informatics_contracts %>%
  describe_contracts_by(contract_value_bin)

informatics_contracts %>%
  describe_contracts_by(contract_value_bin)


## describe pattern of amendment (no change in value, increased cost, decreased cost)
describe_amendment_patterns_by <- function(contracts_to_describe, ...) {
  contracts_to_describe %>%
    filter(amendment_number == "000") %>%
    select(contract_number, amendment_number, award_date, expiry_date, contract_value, total_contract_value, ...) %>%
    mutate(contract_fate = case_when(
      total_contract_value == contract_value ~ "no change in value",
      total_contract_value > contract_value ~ "increased cost",
      total_contract_value < contract_value ~ "decreased cost"
    )) %>%
    group_by(..., contract_fate) %>%
    summarize(
      count = n(),
      total_original = sum(contract_value),
      total_amended = sum(total_contract_value)
    ) %>%
    mutate(
      prop = count / sum(count),
      total_change = total_amended - total_original
    ) %>%
    select(
      ..., contract_fate:count, prop, total_original:total_change
    )
}

software_contracts %>%
  describe_amendment_patterns_by()

software_contracts %>%
  describe_amendment_patterns_by(contract_value_bin)

informatics_contracts %>%
  describe_amendment_patterns_by()

informatics_contracts %>%
  describe_amendment_patterns_by(contract_value_bin)
