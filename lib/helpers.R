identify_fr_columns <- function(dataset) {
  dataset %>%
    names() %>%
    tibble::enframe(name = NULL) %>%
    filter(str_detect(value, "fr$")) %>%
    pull()
}

remove_extra_columns <- function(dataset) {
  dataset %>%
    select(-one_of(identify_empty_columns(.))) %>%
    select(-one_of(identify_fr_columns(.)))
}
