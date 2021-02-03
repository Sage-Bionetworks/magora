library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(forcats)
library(fs)
library(purrr)
library(stringr)
library(lubridate)

# Download and read phenotype data ----

# Read file that contains the phenotype name, synapse ID, field to use, and values of Stain to filter for
phenotype_files <- read_csv(here::here("data-raw", "pathology", "5xfad_data_sources.csv"))

# The code below is commented out because it downloads the files - if they have already been downloaded, no need to run this.

# library(synapser)
# synLogin()
#
# # Download data
#
# phenotype_distinct_files <- phenotype_files %>%
#   distinct(syn_id, version)
#
# walk2(phenotype_distinct_files[["syn_id"]], phenotype_distinct_files[["version"]], ~ synapser::synGet(.x, version = .y, downloadLocation = here::here("data-raw", "pathology"), ifcollision = "overwrite.local"))

# Function to read in, filter + clean data, and return relevant fields
read_clean_phenotype <- function(field, stain_filter, file) {
  res <- readr::read_csv(here::here("data-raw", "pathology", file), na = c("N/A", ""))

  res <- res %>%
    janitor::clean_names()

  if (!is.na(stain_filter)) {
    res <- res %>%
      dplyr::filter(stain == stain_filter)
  }

  res %>%
    dplyr::select(tidyselect::all_of(c("individual_id", "specimen_id", value = janitor::make_clean_names(field)))) %>%
    dplyr::mutate_all(as.character) %>%
    janitor::remove_empty("rows")
}

# Read in data

phenotype_data <- phenotype_files %>%
  mutate(data = pmap(list(field, stain_filter, file), read_clean_phenotype)) %>%
  select(syn_id, phenotype, data) %>%
  unnest(cols = data) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

# Metadata ----

## Biospecimen metadata ----

# synGet("syn18876530", version = 6, downloadLocation = here::here("data-raw", "pathology"))

biospecimen_metadata <- read_csv(here::here("data-raw", "pathology", "UCI_5XFAD_biospecimen_metadata.csv")) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, specimen_id, tissue)

## Individual metadata ----

# synGet("syn18880070", version = 6, downloadLocation = here::here("data-raw", "pathology"))

individual_metadata <- read_csv(here::here("data-raw", "pathology", "UCI_5XFAD_individual_metadata.csv")) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, sex, genotype, genotype_background, date_birth, date_death)

## Check missing IDs ----

# Check which IDs are missing from metadata
phenotype_data %>%
  anti_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  distinct(individual_id, specimen_id) %>%
  write_csv(here::here("data-raw", "pathology", "mice_missing_from_biospecimen_metadata.csv"))

phenotype_data %>%
  anti_join(individual_metadata, by = "individual_id") %>%
  distinct(individual_id) %>%
  write_csv(here::here("data-raw", "pathology", "mice_missing_from_individual_metadata.csv"))

# Clean data ----

## Derive age, mouse line, etc from individual metadata

# Some different date formats to contend with: excel (e.g. 43154), mdy (e.g. 10/1/18), and ydm (e.g. 2019-29-01)

clean_date <- function(date) {
  case_when(
    nchar(date) == 5 ~ excel_numeric_to_date(as.numeric(date)),
    str_detect(date, "-") ~ ydm(date),
    TRUE ~ mdy(date)
  )
}

individual_metadata <- individual_metadata %>%
  mutate(
    sex = str_to_title(sex),
    sex = as_factor(sex),
    across(c(date_birth, date_death), clean_date),
    age_interval = interval(date_birth, date_death),
    age = round(age_interval / months(1)),
    age_factor = as_factor(age),
    age_factor = fct_reorder(age_factor, age),
    mouse_line = case_when(
      str_ends(genotype, "_hemizygous") ~ str_remove(genotype, "_hemizygous"),
      str_ends(genotype, "_noncarrier") ~ genotype_background
    ),
    mouse_line = as_factor(mouse_line)
  ) %>%
  select(-date_birth, -date_death, -age_interval, -genotype, -genotype_background, -age) %>%
  rename(age = age_factor)

## Combine data ----

# For now, use inner_join() so that we only end up with samples that also have metadata

phenotypes <- phenotype_data %>%
  inner_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  inner_join(individual_metadata, by = "individual_id") %>%
  select(individual_id, specimen_id, mouse_line, sex, age, tissue, phenotype, value)

# Save data ----

usethis::use_data(phenotypes, overwrite = TRUE)

# Separately save tissue available for each phenotype, for easily changing inputs available

phenotype_tissue <- split(phenotypes, phenotypes$phenotype) %>%
  map(function(x) distinct(x, tissue) %>% pull(tissue))

usethis::use_data(phenotype_tissue, overwrite = TRUE)
