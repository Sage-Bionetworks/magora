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

phenotype_paths <- map2(phenotype_distinct_files[["syn_id"]], phenotype_distinct_files[["version"]], ~ synapser::synGet(.x, version = .y, downloadLocation = here::here("data-raw", "pathology"), ifcollision = "overwrite.local"))

# Extract path and combine with phenotype_files df so that full path can be used, in case file name changes, rather than whatever is hardcoded in

phenotype_paths <- phenotype_paths %>%
  map("path") %>%
  map(~ tibble(file = .x))

names(phenotype_paths) <- phenotype_distinct_files[["syn_id"]]

phenotype_paths <- phenotype_paths %>%
  bind_rows(.id = "syn_id")

phenotype_files <- phenotype_files %>%
  left_join(phenotype_paths, by = "syn_id", suffix = c("_hardcoded", ""))

# Function to read in, filter + clean data, and return relevant fields
read_clean_phenotype <- function(field, stain_filter, file) {
  res <- readr::read_csv(file, na = c("N/A", ""))

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
  select(syn_id, phenotype, units, data) %>%
  unnest(cols = data) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

# Pull identifier (c, h, p) out of specimen_id

add_specimen_identifier <- function(data) {
  data %>%
  mutate(specimen_identifier = case_when(
    str_detect(specimen_id, "h") ~ "h",
    str_detect(specimen_id, "c") ~ "c",
    str_detect(specimen_id, "p") ~ "p"
  ))
}

phenotype_data <- phenotype_data %>%
  add_specimen_identifier()

# Metadata ----

## Biospecimen metadata ----

biospecimen_id <- "syn18876530"
biospecimen_version <- 8

check_latest_version(biospecimen_id, biospecimen_version)

biospecimen_metadata_path <- synGet(biospecimen_id, version = biospecimen_version, downloadLocation = here::here("data-raw", "pathology"), ifcollision = "overwrite.local")

biospecimen_metadata <- read_csv(biospecimen_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, specimen_id, tissue)

biospecimen_metadata <- biospecimen_metadata %>%
  add_specimen_identifier()

## Individual metadata ----

individual_id <- "syn18880070"
individual_version <- 12

check_latest_version(individual_id, individual_version)

individual_metadata_path <- synGet(individual_id, version = individual_version, downloadLocation = here::here("data-raw", "pathology"), ifcollision = "overwrite.local")

individual_metadata <- read_csv(individual_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, sex, genotype, genotype_background, individual_common_genotype, age_death, age_death_units)

individual_metadata <- individual_metadata %>%
  tidy_ids()

## Pull identifier information out of specimen_id ----

# Only relevant info is whether it contains "c" or "h" - the rest of the specimen_id info after the individual_id is not needed

# Check that none contain BOTH "h" and "c"

phenotype_data %>%
  filter(
    str_detect(specimen_id, "h"),
    str_detect(specimen_id, "c")
  ) %>%
  nrow() == 0

biospecimen_metadata %>%
  filter(
    str_detect(specimen_id, "h"),
    str_detect(specimen_id, "c")
  ) %>%
  nrow() == 0

# Derive identifier column

phenotype_data <- phenotype_data %>%
  mutate(specimen_identifier = case_when(
    str_detect(specimen_id, "h") ~ "h",
    str_detect(specimen_id, "c") ~ "c"
  ))

biospecimen_metadata <- biospecimen_metadata %>%
  mutate(specimen_identifier = case_when(
    str_detect(specimen_id, "h") ~ "h",
    str_detect(specimen_id, "c") ~ "c"
  ))

# Check identifier is present for all records

phenotype_data %>%
  filter(is.na(specimen_identifier)) %>%
  nrow() == 0

biospecimen_metadata %>%
  filter(is.na(specimen_identifier)) %>%
  nrow() == 0

# Some of the biospecimen have "p" instead, but they're not present in the actual data, so seems fine

# Check there is no duplication of identifiers for a given individual_id

phenotype_data %>%
  distinct(phenotype, individual_id, specimen_id, specimen_identifier) %>%
  get_dupes(phenotype, individual_id, specimen_identifier)

biospecimen_metadata %>%
  get_dupes(individual_id, specimen_identifier)

## Check missing IDs ----

# Check which IDs are missing from metadata
# Join on individual_id and specimen_identifier (c/h) only, instead of using specimen_id
phenotype_data %>%
  anti_join(biospecimen_metadata, by = c("individual_id", "specimen_identifier")) %>%
  distinct(individual_id, specimen_identifier)

# Only the 4XXX series is missing, those will get added into the metadata soon

phenotype_data %>%
  anti_join(individual_metadata, by = "individual_id") %>%
  distinct(individual_id)

# Clean data ----

biospecimen_metadata <- biospecimen_metadata %>%
  mutate(tissue = str_to_title(tissue))

# Check that all age death units are "months"

individual_metadata %>%
  count(age_death_unit) %>%
  pull(age_death_unit) == "months"

# Check ages

individual_metadata %>%
  count(age_death)

## Derive mouse line, etc from individual metadata
# Age at death is now in the metadata file, so we do not need to derive it from anything

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
    mouse_model = individual_common_genotype
  ) %>%
  select(-genotype, -genotype_background) %>%
  rename(age = age_factor)

# Check that mouse model matches method for deriving mouse line
individual_metadata %>% count(mouse_line, mouse_model)

# Recode mouse models

individual_metadata <- individual_metadata %>%
  mutate(
    mouse_model = case_when(
      mouse_model == "5XFAD" ~ "5xFAD",
      mouse_model == "C57BL6J" ~ "C57BL/6J"
    ),
    mouse_model = as_factor(mouse_model)
  )

## Combine data ----

phenotypes <- phenotype_data %>%
  inner_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  inner_join(individual_metadata, by = "individual_id") %>%
  select(individual_id, specimen_id, mouse_model, sex, age, tissue, phenotype, units, value)

# Create a display name for phenotypes (with beta symbol instead of "beta") and one with units to display on Y-Axis:

phenotypes <- phenotypes %>%
  mutate(
    phenotype_display = str_replace(phenotype, "Abeta", "A\u03B2"),
    phenotype_units = glue::glue("{phenotype_display}\n({units})")
  ) %>%
  arrange(phenotype)


# Save data ----

usethis::use_data(phenotypes, overwrite = TRUE)

# Separately save tissue available for each phenotype, for easily changing inputs available

phenotype_tissue <- split(phenotypes, phenotypes$phenotype) %>%
  map(function(x) {
    distinct(x, tissue) %>%
      pull(tissue) %>%
      sort()
  })

usethis::use_data(phenotype_tissue, overwrite = TRUE)
