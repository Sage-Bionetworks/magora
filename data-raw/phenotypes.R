library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(forcats)

phenotypes_raw <- read_excel(here::here("data-raw", "5xfAD Phenotyping Sheet - Finalized - 06-17-2020 - JP.xlsx"),
  sheet = "5xfAD 4-18mo phenotype sheet "
)

phenotypes <- phenotypes_raw %>%
  remove_empty("rows") %>%
  mutate(
    Age = gsub("mon", "", Age),
    Age = as.numeric(Age)
  ) %>%
  arrange(Age) %>%
  mutate(
    Age = as_factor(Age),
    tissue = stringr::str_to_title(Tissue),
    sex = as_factor(Sex)
  ) %>%
  separate(`Mouse Line`, into = c("experiment", "mouse_line_group"), fill = "left", remove = FALSE) %>%
  mutate(
    mouse_line_type = ifelse(`Mouse Line` == mouse_line_group, 1, 2)
  )

mouse_line_order <- phenotypes %>%
  arrange(mouse_line_group) %>%
  distinct(mouse_line_group) %>%
  mutate(mouse_line_order = row_number())

phenotypes <- phenotypes %>%
  left_join(mouse_line_order, by = "mouse_line_group") %>%
  mutate(
    mouse_line_factor = as.numeric(paste0(mouse_line_order, mouse_line_type)),
    `Mouse Line` = case_when(
      `Mouse Line` == "BL6" ~ "C57BL6J",
      `Mouse Line` == "5XfAD;BL6" ~ "5XFAD"
    ),
    `Mouse Line` = fct_reorder(`Mouse Line`, mouse_line_factor)
  ) %>%
  pivot_longer(
    cols = c(`Plaque #`:`Plasma AB 42`),
    names_to = "phenotype",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  select(mouse_id = `mouse ID`, tissue, sex, mouse_line = `Mouse Line`, mouse_line_group, age = Age, phenotype, value)

usethis::use_data(phenotypes, overwrite = TRUE)

phenotype_tissue <- split(phenotypes, phenotypes$phenotype) %>%
  lapply(function(x) distinct(x, tissue) %>% pull(tissue))

usethis::use_data(phenotype_tissue, overwrite = TRUE)
