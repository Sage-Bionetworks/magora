library(synapser)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

synLogin()

# Read and clean up TPM data ----

# tpm_per_gene_synapse <- synGet("syn22108848", downloadLocation = here::here("data-raw"))

tpm_per_gene_raw <- read.table(here::here("data-raw", "tpm_gene_5XFAD.txt"), header = TRUE)

tpm_per_gene <- tpm_per_gene_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "mouse_id",
    names_prefix = "X",
    values_to = "tpm"
  ) %>%
  mutate(mouse_id = as.numeric(mouse_id))

# Read and clean up individual metadata ---

# individual_metadata_synapse <- synGet("syn22103212", downloadLocation = here::here("data-raw"))

individual_metadata_raw <- read_csv(here::here("data-raw", "Jax.IU.Pitt_5XFAD_individual_metadata.csv"))

individual_metadata <- individual_metadata_raw %>%
  mutate(
    mouse_line = case_when(
      str_ends(genotype, "_carrier") ~ str_remove(genotype, "_carrier"),
      str_ends(genotype, "_noncarrier") ~ genotypeBackground
    ),
    mouse_line_group = str_remove(genotype, "_carrier|_noncarrier"),
    across(c(dateBirth, dateDeath), mdy),
    age_interval = interval(dateBirth, dateDeath),
    age = round(age_interval / months(1))) %>%
  select(mouse_id = individualID, mouse_line, mouse_line_group, sex, age)

# Check that all mice with tpm data have metadata

tpm_per_gene %>%
  distinct(mouse_id) %>%
  anti_join(individual_metadata, by = "mouse_id") %>%
  nrow() == 0

# Not sure if as relevant -- not all mice with metadata have tpm data

individual_metadata %>%
  anti_join(
    tpm_per_gene %>%
      distinct(mouse_id),
    by = "mouse_id"
  ) %>%
  nrow() == 0

# Combine tpm with metadata -----

gene_expressions <- tpm_per_gene %>%
  left_join(individual_metadata, by = "mouse_id") %>%
  select(mouse_id, mouse_line, mouse_line_group, sex, age, gene_id, tpm)

usethis::use_data(gene_expressions, overwrite = TRUE)
