source("load.R")

software_gsin_descs_excl_maintenance <- contracts %>%
  count_group(gsin, gsin_description_en) %>%
  ungroup() %>%
  filter(str_detect(gsin_description_en, regex("software", ignore_case = TRUE))) %>%
  filter(! str_detect(gsin_description_en, regex("maintenance", ignore_case = TRUE))) %>%
  pull(gsin_description_en)

software_contract_numbers_with_amendment_000 <- contracts %>%
  filter(gsin_description_en %in% software_gsin_descs_excl_maintenance) %>%
  select(contract_number, amendment_number) %>%
  filter(amendment_number == "000") %>%
  pull(contract_number)

software_contracts_in_scope <- contracts %>%
  filter(contract_number %in% software_contract_numbers_with_amendment_000) %>%
  mutate(contract_length = time_length(interval(award_date, expiry_date), "months")) %>%
  mutate(contract_length_bin = cut(
    contract_length,
    breaks = c(-Inf, 0, 6, 12, Inf),
    labels = c("negative length", "<6 mos", "6 mos to 1 yr", ">1 yr")
  ))

# double check that summing the `contract_value` column comes out to the same count as `total_contract_value`
software_contracts_in_scope %>%
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

software_contracts_in_scope %>%
  group_by(contract_length_bin) %>%
  summarize(
    count = n(),
    total = sum(contract_value)
  )
