library(synapser)
library(readr)
library(AnnotationDbi)
library(EnsDb.Mmusculus.v79)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(purrr)

source(here::here("data-raw/gene_expressions_data_functions.R"))

options(scipen = 999) # So that casting mouse_id to character doesn't convert e.g. 10000000 to "1e+07"

# synLogin()

# Read and clean up TPM data ----

# 5XFAD ----

# synGet("syn22108848", downloadLocation = here::here("data-raw", "gene_expressions", "5xfad"))

tpm_5xfad <- read.table(here::here("data-raw", "gene_expressions", "5xfad", "tpm_gene_5XFAD.txt"), header = TRUE) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "specimen_id",
    names_prefix = "X",
    values_to = "value"
  ) %>%
  mutate(mouse_id = str_remove(specimen_id, "rh"))

# htau_trem2 ----

# download_tpm_and_annotations("syn18694013", "htau_trem2")

# Read and combine htau_trem2 data
# tpm_htau_trem2 <- read_combine_tpm_and_annotations("htau_trem2")

# app_ps1 ----

# download_tpm_and_annotations("syn15666853", "app_ps1")

# Read and combine app_ps1 data
tpm_app_ps1 <- read_combine_tpm_and_annotations("app_ps1")

### combine tpm

tpm <- tpm_5xfad %>%
  # bind_rows(tpm_htau_trem2) %>%
  bind_rows(tpm_app_ps1)

# Individual Metadata ----

# 5XFAD ----

#synGet("syn22103212", downloadLocation = here::here("data-raw"))

# Logic for mouse line:
# if genotype is 5XFAD_carrier, then Mouse Line = 5XFAD
# If genotype is 5XFAD_noncarrier, then Mouse Line = genotypeBackground

individual_metadata_5xfad <- read_csv(here::here("data-raw", "gene_expressions", "5xfad", "Jax.IU.Pitt_5XFAD_individual_metadata.csv")) %>%
  mutate(
    sex = str_to_title(sex),
    mouse_line = case_when(
      str_ends(genotype, "_carrier") ~ str_remove(genotype, "_carrier"),
      str_ends(genotype, "_noncarrier") ~ genotypeBackground
    ),
    across(c(dateBirth, dateDeath), mdy),
    age_interval = interval(dateBirth, dateDeath),
    age = round(age_interval / months(1)),
    mouse_id = as.character(individualID)
  ) %>%
  select(mouse_id, mouse_line, sex, age)

# htau_trem2 ----

# synGet("syn22161041", downloadLocation = here::here("data-raw", "gene_expressions", "htau_trem2"))

# individual_metadata_htau_trem2 <- read_csv(here::here("data-raw", "gene_expressions", "htau_trem2", "Jax.IU.Pitt_hTau_Trem2_individual_metdata.csv")) %>%
#   mutate(
#     sex = str_to_title(sex),
#     mouse_line = genotype,
#     across(c(dateBirth, dateDeath), mdy),
#     age_interval = interval(dateBirth, dateDeath),
#     age = round(age_interval / months(1)),
#     age = ifelse(age == -6, 6, age) # Fix one age where dateBirth and dateDeath look mixed up
#   ) %>%
#   select(mouse_id = individualID, mouse_line, sex, age)

# app_ps1 ----

# synGet("syn18879639", downloadLocation = here::here("data-raw", "gene_expressions", "app_ps1"))

individual_metadata_app_ps1 <- read_csv(here::here("data-raw", "gene_expressions", "app_ps1", "Jax.IU.Pitt_APP.PS1_individual_metadata.csv")) %>%
  mutate(
    sex = str_to_title(sex),
    mouse_line = genotype,
    # Age is in two columns: either there is ageOfDeath and no dateBirth/dateDeath, or the opposite
    # Calculate both and then coalesce
    age_m = as.numeric(str_remove(ageOfDeath, "m")),
    age_interval = interval(dateBirth, dateDeath),
    age_d = round(age_interval / months(1)),
    age = coalesce(age_m, age_d),
    mouse_id = as.character(individualID)
  ) %>%
  select(mouse_id, mouse_line, sex, age)

# Combine individual metadata

individual_metadata <- individual_metadata_5xfad %>%
  # bind_rows(individual_metadata_htau_trem2) %>%
  bind_rows(individual_metadata_app_ps1)

# Check there are no duplicate mouse_ids across the datasets

individual_metadata %>%
  add_count(mouse_id) %>%
  filter(n > 1) %>%
  nrow() == 0

# Biospecimen metadata ----

# 5xfad ----

# synGet("syn22103213", downloadLocation = here::here("data-raw", "gene_expressions", "5xfad"))

biospecimen_metadata_5xfad <- read_csv(here::here("data-raw", "gene_expressions", "5xfad", "Jax.IU.Pitt_5XFAD_biospecimen_metadata.csv"))

tissue_5xfad <- tpm_5xfad %>%
  distinct(mouse_id, specimen_id) %>%
  left_join(biospecimen_metadata_5xfad, by = c("specimen_id" = "specimenID")) %>%
  select(mouse_id, specimen_id, tissue)

# htau_trem2 ----

# synGet("syn18720956", downloadLocation = here::here("data-raw", "gene_expressions", "htau_trem2"))

# biospecimen_metadata_htau_trem2 <- read_csv(here::here("data-raw", "gene_expressions", "htau_trem2", "Jax.IU.Pitt_hTau_Trem2_biospecimen_metadata.csv"))

# tissue_htau_trem2 <- tpm_htau_trem2 %>%
#   distinct(mouse_id, specimen_id) %>%
#   left_join(biospecimen_metadata_htau_trem2, by = c("mouse_id" = "individualID", "specimen_id" = "specimenID")) %>%
#   select(mouse_id, specimen_id, tissue)

# app_ps1 -----

# synGet("syn18879638", downloadLocation = here::here("data-raw", "gene_expressions", "app_ps1"))

biospecimen_metadata_app_ps1 <- read_csv(here::here("data-raw", "gene_expressions", "app_ps1", "Jax.IU.Pitt_APP.PS1_biospecimen_metadata.csv")) %>%
  mutate(individualID = as.character(individualID),
         specimenID = as.character(specimenID))

tissue_app_ps1 <- tpm_app_ps1 %>%
  distinct(mouse_id, specimen_id) %>%
  left_join(biospecimen_metadata_app_ps1, by = c("mouse_id" = "individualID", "specimen_id" = "specimenID")) %>%
  select(mouse_id, specimen_id, tissue)

# Combine tissue

tissue <- tissue_5xfad %>%
  # bind_rows(tissue_htau_trem2) %>%
  bind_rows(tissue_app_ps1)

# Query symbols to use in place of gene ids where possible ----

genes <- tpm %>%
  distinct(gene_id)

gene_symbols <- AnnotationDbi::select(EnsDb.Mmusculus.v79, keys = genes[["gene_id"]], columns = "SYMBOL", keytype = "GENEID") %>%
  as_tibble() %>%
  rename(gene_id = GENEID, gene_symbol = SYMBOL) %>%
  mutate(gene_symbol = ifelse(gene_symbol %in% c(""), NA_character_, gene_symbol))

# If the symbol exists, use that - otherwise, use gene id

genes <- genes %>%
  left_join(gene_symbols, by = "gene_id") %>%
  mutate(gene = coalesce(gene_symbol, gene_id))

# There are some cases where one symbol corresponds to many gene ids - rather than collapsing them, also fall back to the gene id instead of the symbol

genes <- genes %>%
  add_count(gene_symbol) %>%
  mutate(gene = case_when(
    n > 1 ~ gene_id,
    TRUE ~ gene
  )) %>%
  select(-n)

# Check that there aren't multiple symbols for a single gene

genes %>%
  add_count(gene_id) %>%
  filter(n > 1) %>%
  nrow() == 0

# Combine tpm, individual metadata, tissue, and gene symbol ----

gene_expressions <- tpm %>%
  left_join(individual_metadata, by = "mouse_id") %>%
  left_join(tissue, by = c("mouse_id", "specimen_id")) %>%
  left_join(genes, by = "gene_id") %>%
  select(mouse_id, specimen_id, mouse_line, sex, age, tissue, gene, value)

# Check no rows have been added via duplicate IDs etc in joins

nrow(tpm) == nrow(gene_expressions)

# Remove one mouse with age 22, likely an error

gene_expressions <- gene_expressions %>%
  filter(age != 22)

# Sample of 10,000 genes ----
# Sampling by % of zeros to get a good idea of what plots will look like

set.seed(1234)

sample_genes <- gene_expressions %>%
  mutate(zero = value == 0) %>%
  group_by(gene) %>%
  summarise(prop_zero = mean(zero)) %>%
  mutate(prop_zero_group = cut(prop_zero, breaks = seq(0, 1, 0.25), include.lowest = TRUE)) %>%
  group_by(prop_zero_group) %>%
  sample_n(2500) %>%
  pull(gene)

gene_expressions <- gene_expressions %>%
  filter(gene %in% sample_genes)

# Make variables into factors to save space when saving, remove unused columns

gene_expressions <- gene_expressions %>%
  arrange(age) %>%
  mutate(across(c(sex, age, tissue, gene, mouse_line), .fns = as_factor)) %>%
  select(-mouse_id, -specimen_id)

# Save data ----

# Saving mouse line, genes, and tissues separately to be used as inputs - tried out generating them via renderUI() but there's a considerable slowdown versus saving as objects directly

gene_expression_genes <- sort(levels(gene_expressions[["gene"]]))
usethis::use_data(gene_expression_genes, overwrite = TRUE)

gene_expression_mouse_lines <- c("C57BL6J", "5XFAD", "APP/PS1_hemizygous")
usethis::use_data(gene_expression_mouse_lines, overwrite = TRUE)

gene_expression_tissues <- sort(levels(gene_expressions[["tissue"]]))
usethis::use_data(gene_expression_tissues, overwrite = TRUE)

# Save tissues relevant for each mouse line to update selector
gene_expressions_mouse_line_tissues <- split(gene_expressions, gene_expressions$mouse_line) %>%
  lapply(function(x) distinct(x, tissue) %>% pull(tissue) %>% as.character())
usethis::use_data(gene_expressions_mouse_line_tissues, overwrite = TRUE)

saveRDS(gene_expressions, here::here("inst", "extdata", "gene_expressions.rds"))
