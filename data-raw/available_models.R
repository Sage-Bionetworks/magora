library(readr)
library(dplyr)

available_models <- read_csv(here::here("data-raw", "available_models", "available_models.csv"))

available_models <- available_models %>%
  mutate(
    `Source Data` = `Source Data URL`,
    `Model Information` = ModelInformationURL
  ) %>%
  select(-`Source Data URL`, -ModelInformationURL)

usethis::use_data(available_models, overwrite = TRUE)
