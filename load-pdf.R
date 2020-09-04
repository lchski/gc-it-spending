# https://www.ourcommons.ca/Committees/en/OGGO/StudyActivity?studyActivityId=10769410
# https://smallpdf.com/result#r=c49f0c682bf3d3bd9e245cda4a19b398&t=pdf-to-excel

library(readxl)

expenditures_201819 <- tibble(sheet = 2:44) %>%
  mutate(raw_excel = map(sheet, ~ read_excel("data/source/TreasuryBoardSecretariat-2-e-converted.xlsx", .x)))

remove_extra_rows_from_raw_excel <- function(sheet, raw_excel) {
  if (sheet %in% c(10, 16)) { ## these two have a bunch of extra rows for some reason
    return(
      raw_excel %>% slice(1:27)
    )
  } else {
    return(
      raw_excel
    )
  }
}

remove_header_row_from_raw_excel <- function(sheet, raw_excel) {
  if (nrow(raw_excel) > 27 & ! sheet %in% c(19)) { ## these added an extra header row
    return(
      raw_excel %>% slice(-1)
    )
  } else {
    return(
      raw_excel
    )
  }
}

remove_extra_last_row_from_raw_excel <- function(sheet, raw_excel) {
  if (nrow(raw_excel) == 28 & sheet %in% c(7, 19)) { ## these two have extra last rows
    return(
      raw_excel %>% slice(-28)
    )
  } else {
    return(
      raw_excel
    )
  }
}

expenditures_201819 %>%
  mutate(raw_excel = map2(sheet, raw_excel, remove_extra_rows_from_raw_excel)) %>%
  mutate(raw_excel = map2(sheet, raw_excel, remove_header_row_from_raw_excel)) %>%
  mutate(raw_excel = map2(sheet, raw_excel, remove_extra_last_row_from_raw_excel)) %>%
  mutate(dept = map_chr(raw_excel, ~ .[[2,2]])) %>%
  mutate(spend_data = map(raw_excel, ~ (.) %>% slice(-1:-6))) ## remove first six rows, all metadata


read_excel("data/source/TreasuryBoardSecretariat-2-e-converted.xlsx", 3) %>% .[[2,2]]
