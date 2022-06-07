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

# 5xFAD ----

model <- "5xFAD"

## Download and read phenotype data ----

# Read file that contains the phenotype name, synapse ID, field to use, and values of Stain to filter for
phenotype_files <- read_csv(here::here("data-raw", "pathology", model, glue::glue("{model}_data_sources.csv")))

# Check that the latest version of all files is used
walk2(phenotype_files[["syn_id"]], phenotype_files[["version"]], check_latest_version)

# Download data - synapser checks the version locally and does not redownload if the version is up to date, so we can safely run all of this without worrying about redownloading.

phenotype_distinct_files <- phenotype_files %>%
  distinct(syn_id, version)

phenotype_paths <- map2(phenotype_distinct_files[["syn_id"]], phenotype_distinct_files[["version"]], ~ synapser::synGet(.x, version = .y, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local"))

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
  mutate(
    value = as.numeric(value),
    # Convert any NAs to 0s for 5xFAD case only
    value = coalesce(value, 0)
  )

## Metadata ----

### Biospecimen metadata ----

biospecimen_id <- "syn18876530"
biospecimen_version <- 9

check_latest_version(biospecimen_id, biospecimen_version)

biospecimen_metadata_path <- synGet(biospecimen_id, version = biospecimen_version, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local")

biospecimen_metadata <- read_csv(biospecimen_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, specimen_id, tissue)

#### Check metadata has correct tissue identifier ---

correct_tissue_identifier <- tibble::tribble(
  ~tissue, ~specimen_identifier,
  "hippocampus", "h",
  "cerebral cortex", "c",
  "plasma", "p"
)

biospecimen_metadata %>%
  mutate(
    specimen_id = tolower(specimen_id),
    specimen_identifier = case_when(
      str_detect(specimen_id, "c") ~ "c",
      str_detect(specimen_id, "h") ~ "h",
      str_detect(specimen_id, "p") ~ "p"
    )
  ) %>%
  anti_join(correct_tissue_identifier, by = c("tissue", "specimen_identifier"))

# There is only one issue, 463rc - but it does not appear in the actual data, so no issue.

phenotype_data %>%
  filter(specimen_id == "463rc")

### Individual metadata ----

individual_id <- "syn18880070"
individual_version <- 12

check_latest_version(individual_id, individual_version)

individual_metadata_path <- synGet(individual_id, version = individual_version, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local")

individual_metadata <- read_csv(individual_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, sex, genotype, genotype_background, individual_common_genotype, age_death, age_death_units)

### Check missing IDs ----

# Check which IDs are missing from metadata
phenotype_data %>%
  anti_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  distinct(individual_id, specimen_id)

# 290Icdf and 278Icis are known to be missing - for these two, use the fact that the "c" in the identifier is "cerebral cortex", and manually add it to the metadata

missing_biospecimen_metdata <- tribble(
  ~individual_id, ~specimen_id, ~tissue,
  "290", "290Icdf", "cerebral cortex",
  "278", "278Icis", "cerebral cortex"
)

biospecimen_metadata <- biospecimen_metadata %>%
  bind_rows(
    missing_biospecimen_metdata
  )

phenotype_data %>%
  anti_join(individual_metadata, by = "individual_id") %>%
  distinct(individual_id)

## Clean data ----

biospecimen_metadata <- biospecimen_metadata %>%
  mutate(tissue = str_to_title(tissue))

# Check that all age death units are "months"

individual_metadata %>%
  count(age_death_units) %>%
  pull(age_death_units) == "months"

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

### Combine data ----

phenotypes_5xfad <- phenotype_data %>%
  inner_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  inner_join(individual_metadata, by = "individual_id") %>%
  select(individual_id, specimen_id, mouse_model, sex, age, tissue, phenotype, units, value)

# 3xTg ----

model <- "3xTg-AD"

## Download and read  data ----

# Read file that contains the phenotype name, synapse ID, field to use, and values of Stain to filter for
phenotype_files <- read_csv(here::here("data-raw", "pathology", model, glue::glue("{model}_data_sources.csv")))

# Check that the latest version of all files is used
walk2(phenotype_files[["syn_id"]], phenotype_files[["version"]], check_latest_version)

# Download data - synapser checks the version locally and does not redownload if the version is up to date, so we can safely run all of this without worrying about redownloading

phenotype_distinct_files <- phenotype_files %>%
  distinct(syn_id, version)

phenotype_paths <- map2(phenotype_distinct_files[["syn_id"]], phenotype_distinct_files[["version"]], ~ synapser::synGet(.x, version = .y, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local"))

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

  res <- res %>%
    # Some will not have individual_id because they have a Pool identifier instead - that's okay, just grab it if it is there
    dplyr::select(tidyselect::any_of(c("individual_id")), tidyselect::all_of(c("specimen_id", value = janitor::make_clean_names(field)))) %>%
    dplyr::mutate_all(as.character) %>%
    janitor::remove_empty("rows")

  # If it's not there, add a "fake" one just for keeping track of some ID

  if (!"individual_id" %in% names(res)) {
    res <- res %>%
      mutate(individual_id = as.character(row_number()))
  }

  res
}

# Read in data

phenotype_data <- phenotype_files %>%
  mutate(data = pmap(list(field, stain_filter, file), read_clean_phenotype)) %>%
  select(syn_id, phenotype, units, data) %>%
  unnest(cols = data) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

## Metadata ----

### Biospecimen metadata ----

biospecimen_id <- "syn23532198"
biospecimen_version <- 3

check_latest_version(biospecimen_id, biospecimen_version)

biospecimen_metadata_path <- synGet(biospecimen_id, version = biospecimen_version, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local")

biospecimen_metadata <- read_csv(biospecimen_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, specimen_id, tissue)

### Individual metadata ----

individual_id <- "syn23532199"
individual_version <- 4

check_latest_version(individual_id, individual_version)

individual_metadata_path <- synGet(individual_id, version = individual_version, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local")

individual_metadata <- read_csv(individual_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, sex, genotype, genotype_background, individual_common_genotype, age_death, age_death_units)

### Check missing IDs ----

# Check which IDs are missing from metadata
phenotype_data %>%
  filter(!str_starts(specimen_id, "Pool")) %>%
  anti_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  distinct(individual_id, specimen_id)

phenotype_data %>%
  filter(!str_starts(specimen_id, "Pool")) %>%
  anti_join(individual_metadata, by = "individual_id") %>%
  distinct(individual_id)

## Clean data ----

biospecimen_metadata <- biospecimen_metadata %>%
  mutate(tissue = str_to_title(tissue))

# Check that all age death units are "months"

individual_metadata %>%
  count(age_death_units) %>%
  pull(age_death_units) == "months"

# Check ages

individual_metadata %>%
  count(age_death)

## Derive features

individual_metadata <- individual_metadata %>%
  mutate(
    sex = str_to_title(sex),
    sex = as_factor(sex),
    age_factor = as_factor(age_death),
    age_factor = fct_reorder(age_factor, age_death),
    mouse_model = individual_common_genotype
  ) %>%
  select(-genotype, -genotype_background) %>%
  rename(age = age_factor)

### Form metadata for Pool identified specimens ----

pool_biospecimen_metadata <- biospecimen_metadata %>%
  filter(str_starts(specimen_id, "Pool"))

pool_metadata <- pool_biospecimen_metadata %>%
  left_join(individual_metadata, by = "individual_id") %>%
  select(-individual_id) %>%
  distinct() %>%
  arrange(specimen_id)

# Check that there is only one row for each pool

pool_metadata %>%
  get_dupes(specimen_id)

### Combine data with metadata ----

phenotypes_non_pool <- phenotype_data %>%
  filter(!str_starts(specimen_id, "Pool")) %>%
  inner_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  inner_join(individual_metadata, by = "individual_id") %>%
  select(individual_id, specimen_id, mouse_model, sex, age, tissue, phenotype, units, value)

phenotypes_pool <- phenotype_data %>%
  filter(str_starts(specimen_id, "Pool")) %>%
  inner_join(pool_metadata, by = "specimen_id") %>%
  select(individual_id, specimen_id, mouse_model, sex, age, tissue, phenotype, units, value)

phenotypes_3xtg <- phenotypes_non_pool %>%
  bind_rows(phenotypes_pool)

# Check none were lost
nrow(phenotypes_3xtg) == nrow(phenotype_data)

# Manually recode control to B6129

phenotypes_3xtg <- phenotypes_3xtg %>%
  mutate(mouse_model = ifelse(mouse_model == "B6129F3", "B6129", mouse_model))

# Trem2-R47H_NSS ----

model <- "Trem2-R47H_NSS"

## Download and read phenotype data ----

# Read file that contains the phenotype name, synapse ID, field to use, and values of Stain to filter for
phenotype_files <- read_csv(here::here("data-raw", "pathology", model, glue::glue("{model}_data_sources.csv")))

# Check that the latest version of all files is used
walk2(phenotype_files[["syn_id"]], phenotype_files[["version"]], check_latest_version)

# Download data - synapser checks the version locally and does not redownload if the version is up to date, so we can safely run all of this without worrying about redownloading.

phenotype_distinct_files <- phenotype_files %>%
  distinct(syn_id, version)

phenotype_paths <- map2(phenotype_distinct_files[["syn_id"]], phenotype_distinct_files[["version"]], ~ synapser::synGet(.x, version = .y, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local"))

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

## Metadata ----

### Biospecimen metadata ----

biospecimen_id <- "syn29568452"
biospecimen_version <- 1

check_latest_version(biospecimen_id, biospecimen_version)

biospecimen_metadata_path <- synGet(biospecimen_id, version = biospecimen_version, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local")

biospecimen_metadata <- read_csv(biospecimen_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, specimen_id, tissue)

#### Check metadata has correct tissue identifier ---

correct_tissue_identifier <- tibble::tribble(
  ~tissue, ~specimen_identifier,
  "hippocampus", "h",
  "cerebral cortex", "c",
  "plasma", "p"
)

biospecimen_metadata %>%
  mutate(
    specimen_id = tolower(specimen_id),
    specimen_identifier = case_when(
      str_detect(specimen_id, "c") ~ "c",
      str_detect(specimen_id, "h") ~ "h",
      str_detect(specimen_id, "p") ~ "p"
    )
  ) %>%
  anti_join(correct_tissue_identifier, by = c("tissue", "specimen_identifier"))

### Individual metadata ----

individual_id <- "syn27147690"
individual_version <- 2

check_latest_version(individual_id, individual_version)

individual_metadata_path <- synGet(individual_id, version = individual_version, downloadLocation = here::here("data-raw", "pathology", model), ifcollision = "overwrite.local")

individual_metadata <- read_csv(individual_metadata_path[["path"]]) %>%
  mutate(individualID = as.character(individualID)) %>%
  clean_names() %>%
  select(individual_id, sex, genotype, genotype_background, individual_common_genotype, age_death, age_death_units)

### Check missing IDs ----

# Check which IDs are missing from metadata
phenotype_data %>%
  anti_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  distinct(individual_id, specimen_id)

phenotype_data %>%
  anti_join(individual_metadata, by = "individual_id") %>%
  distinct(individual_id)

## Clean data ----

biospecimen_metadata <- biospecimen_metadata %>%
  mutate(tissue = str_to_title(tissue))

# Check that all age death units are "months"

individual_metadata %>%
  count(age_death_units) %>%
  pull(age_death_units) == "months"

# Check ages

individual_metadata %>%
  count(age_death)

## Clean up factors etc

individual_metadata <- individual_metadata %>%
  mutate(
    sex = str_to_title(sex),
    sex = as_factor(sex),
    age_factor = as_factor(age_death),
    age_factor = fct_reorder(age_factor, age_death),
    mouse_model = individual_common_genotype
  ) %>%
  select(-genotype, -genotype_background, -individual_common_genotype) %>%
  rename(age = age_factor)

# Recode mouse models

individual_metadata <- individual_metadata %>%
  mutate(
    mouse_model = str_replace_all(mouse_model, "5XFAD", "5xFAD"),
    mouse_model = str_replace_all(mouse_model, "C57BL6J", "C57BL/6J"),
    mouse_model = as_factor(mouse_model)
  )

### Combine data ----

phenotypes_trem2_r47h_nss <- phenotype_data %>%
  inner_join(biospecimen_metadata, by = c("individual_id", "specimen_id")) %>%
  inner_join(individual_metadata, by = "individual_id") %>%
  select(individual_id, specimen_id, mouse_model, sex, age, tissue, phenotype, units, value)

# Combine models ----
# Also set mouse model factor order, for plots (individually for each model)

pathology <- list(
  "5xFAD" = phenotypes_5xfad %>%
    mutate(
      mouse_model_group = "5xFAD",
      mouse_model = fct_relevel(mouse_model, c(
        "C57BL/6J", "5xFAD"
      ))
    ),
  "3xTg-AD" = phenotypes_3xtg %>%
    mutate(
      mouse_model_group = "3xTg-AD",
      mouse_model = fct_relevel(mouse_model, c(
        "B6129", "3xTg-AD"
      ))
    ),
  "Trem2-R47H_NSS" = phenotypes_trem2_r47h_nss %>%
    mutate(
      mouse_model_group = "Trem2-R47H_NSS",
      mouse_model = fct_relevel(mouse_model, c(
        "5xFADTrem2-R47H_NSS", "Trem2-R47H_NSS", "5xFAD", "C57BL/6J"
      ))
    )
)

# Set age factor (collectively, for all data)

age_levels <- pathology %>%
  bind_rows() %>%
  pull(age) %>%
  levels()

pathology <- pathology %>%
  map(function(x) {
    x %>%
      mutate(
        age = fct_expand(age, age_levels),
        age = fct_relevel(age, age_levels)
      )
  })

# Create a display name for phenotypes (with beta symbol instead of "beta") and one with units to display on Y-Axis:

pathology <- pathology %>%
  map(function(x) {
    x %>%
      mutate(
        phenotype_display = str_replace(phenotype, "Abeta", "A\u03B2"),
        phenotype_units = glue::glue("{phenotype_display}\n({units})")
      ) %>%
      arrange(phenotype)
  })

# Save data ----

usethis::use_data(pathology, overwrite = TRUE)

# Separately save tissue available for each phenotype, for easily changing inputs available

pathology_tissue <- pathology %>%
  bind_rows() %>%
  split(.$phenotype) %>%
  map(function(x) {
    distinct(x, tissue) %>%
      pull(tissue) %>%
      sort()
  })

usethis::use_data(pathology_tissue, overwrite = TRUE)

# Separately save mouse model (models and control) for each group, for easily selecting just model and getting all

pathology_mouse_models <- pathology %>%
  map(function(x) {
    x %>%
      arrange(mouse_model) %>%
      pull(mouse_model) %>%
      unique() %>%
      as.character()
  })

usethis::use_data(pathology_mouse_models, overwrite = TRUE)
