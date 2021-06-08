library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(forcats)
library(fs)
library(purrr)
library(stringr)
library(lubridate)
library(synapser)

synLogin()

source(here::here("data-raw", "check_latest_version.R"))

# Download and read phenotype data ----

# Read file that contains the phenotype name, synapse ID, field to use, and values of Stain to filter for
phenotype_files <- read_csv(here::here("data-raw", "pathology", "5xfad_data_sources.csv"))

# Check that the latest version of all files is used
walk2(phenotype_files[["syn_id"]], phenotype_files[["version"]], check_latest_version)

# Download data - synapser checks the version locally and does not redownload if the version is up to date, so we can safely run all of this without worrying about redownloading.

phenotype_distinct_files <- phenotype_files %>%
  distinct(syn_id, version)

walk2(phenotype_distinct_files[["syn_id"]], phenotype_distinct_files[["version"]], ~ synapser::synGet(.x, version = .y, downloadLocation = here::here("data-raw", "pathology"), ifcollision = "overwrite.local"))

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

biospecimen_id <- "syn18876530"
biospecimen_version <- 8

check_latest_version(biospecimen_id, biospecimen_version)

synGet(biospecimen_id, version = biospecimen_version, downloadLocation = here::here("data-raw", "pathology"))

biospecimen_metadata <- read_csv(here::here("data-raw", "pathology", "UCI_5XFAD_biospecimen_metadata.csv")) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, specimen_id, tissue)

## Individual metadata ----

individual_id <- "syn18880070"
individual_version <- 9

check_latest_version(individual_id, individual_version)

synGet(individual_id, version = individual_version, downloadLocation = here::here("data-raw", "pathology"))

individual_metadata <- read_csv(here::here("data-raw", "pathology", "UCI_5XFAD_individual_metadata.csv")) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, sex, genotype, genotype_background, individual_common_genotype, age_death, age_death_unit)

## Check missing IDs ----

# Check which IDs are missing from metadata
phenotype_data %>%
  anti_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  distinct(individual_id, specimen_id)

phenotype_data %>%
  anti_join(individual_metadata, by = "individual_id") %>%
  distinct(individual_id)

# Clean data ----

## Derive mouse line, etc from individual metadata
# Age at death is now in the metadata file, so we do not need to derive it from anything

# Check that all age death units are "months"

individual_metadata %>%
  count(age_death_unit) %>%
  pull(age_death_unit) == "months"

# Fix one age of death that is 181 instead of 18 - update with new metadata when fixed
individual_metadata <- individual_metadata %>%
  mutate(age_death = ifelse(age_death == 181, 18, age_death))

individual_metadata <- individual_metadata %>%
  mutate(
    sex = str_to_title(sex),
    sex = as_factor(sex),
    age_factor = as_factor(age_death),
    age_factor = fct_reorder(age_factor, age_death),
    mouse_line = case_when(
      str_ends(genotype, "_hemizygous") ~ str_remove(genotype, "_hemizygous"),
      str_ends(genotype, "_noncarrier") ~ genotype_background
    ),
    mouse_line = as_factor(mouse_line),
    mouse_model = as_factor(individual_common_genotype)
  ) %>%
  select(-genotype, -genotype_background) %>%
  rename(age = age_factor)

biospecimen_metadata <- biospecimen_metadata %>%
  mutate(tissue = str_to_title(tissue))

individual_metadata %>% count(mouse_line, mouse_model)
# Should they be the same? There are a few that don't match, e.g. I say C57BL6J but the individual_common_genotype says 5XFAD
# Anna will update metadata

## Combine data ----

phenotypes <- phenotype_data %>%
  left_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  left_join(individual_metadata, by = "individual_id") %>%
  select(individual_id, specimen_id, mouse_model, sex, age, tissue, phenotype, value)

# Save data ----

usethis::use_data(phenotypes, overwrite = TRUE)

# Separately save tissue available for each phenotype, for easily changing inputs available

phenotype_tissue <- split(phenotypes, phenotypes$phenotype) %>%
  map(function(x) distinct(x, tissue) %>% pull(tissue) %>% sort())

usethis::use_data(phenotype_tissue, overwrite = TRUE)
