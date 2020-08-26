library(synapser)
library(readr)
library(AnnotationDbi)
library(EnsDb.Mmusculus.v79)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)

# synLogin()

# Read and clean up TPM data ----

# tpm_per_gene_synapse <- synGet("syn22108848", downloadLocation = here::here("data-raw"))

tpm_per_gene_raw <- read.table(here::here("data-raw", "tpm_gene_5XFAD.txt"), header = TRUE)

tpm_per_gene <- tpm_per_gene_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "mouse_id",
    names_prefix = "X",
    values_to = "value"
  ) %>%
  mutate(mouse_id = as.numeric(mouse_id))

# Query symbols to use in place of gene ids where possible ----

genes <- tpm_per_gene %>%
  distinct(gene_id)

gene_symbols <- AnnotationDbi::select(EnsDb.Mmusculus.v79, keys = genes[["gene_id"]], columns = "SYMBOL", keytype = "GENEID") %>%
  as_tibble() %>%
  rename(gene_id = GENEID, gene_symbol = SYMBOL) %>%
  mutate(gene_symbol = ifelse(gene_symbol == "", NA_character_, gene_symbol))

# If the symbol exists, use that - otherwise, use gene id

genes <- genes %>%
  left_join(gene_symbols, by = "gene_id") %>%
  mutate(gene = coalesce(gene_symbol, gene_id))

# There are some cases where one symbol corresponds to many gene ids - rather than collapsing them, also fall back to the gene id instead of the symbol

genes <- genes %>%
  add_count(gene_symbol) %>%
  mutate(gene = case_when(n > 1 ~ gene_id,
                          TRUE ~ gene)) %>%
  select(-n)

# Check that there aren't multiple symbols for a single gene

genes %>%
  add_count(gene_id) %>%
  filter(n > 1) %>%
  nrow() == 0

# Read and clean up individual metadata ---

# individual_metadata_synapse <- synGet("syn22103212", downloadLocation = here::here("data-raw"))

individual_metadata_raw <- read_csv(here::here("data-raw", "Jax.IU.Pitt_5XFAD_individual_metadata.csv"))

individual_metadata <- individual_metadata_raw %>%
  mutate(
    sex = str_to_title(sex),
    mouse_line = case_when(
      str_ends(genotype, "_carrier") ~ str_remove(genotype, "_carrier"),
      str_ends(genotype, "_noncarrier") ~ genotypeBackground
    ),
    mouse_line_type = case_when( # Assuming that "carrier" is the experiment and "noncarrier" is the control
      str_ends(genotype, "_carrier") ~ 2,
      str_ends(genotype, "_noncarrier") ~ 1
    ),
    mouse_line_group = str_remove(genotype, "_carrier|_noncarrier"),
    across(c(dateBirth, dateDeath), mdy),
    age_interval = interval(dateBirth, dateDeath),
    age = round(age_interval / months(1))
  ) %>%
  select(mouse_id = individualID, mouse_line, mouse_line_type, mouse_line_group, sex, age)

# Order mouse line factor so that control shows first

mouse_line_order <- individual_metadata %>%
  arrange(mouse_line_group) %>%
  distinct(mouse_line_group) %>%
  mutate(mouse_line_order = row_number())

individual_metadata <- individual_metadata %>%
  left_join(mouse_line_order, by = "mouse_line_group") %>%
  mutate(
    mouse_line_factor = as.numeric(paste0(mouse_line_order, mouse_line_type)),
    mouse_line = fct_reorder(mouse_line, mouse_line_factor)
  ) %>%
  select(-mouse_line_type, -mouse_line_order, -mouse_line_factor)

# Check that all mice with tpm data have metadata

tpm_per_gene %>%
  distinct(mouse_id) %>%
  anti_join(individual_metadata, by = "mouse_id") %>%
  nrow() == 0

# Combine tpm with metadata and gene symbol -----

gene_expressions <- tpm_per_gene %>%
  left_join(individual_metadata, by = "mouse_id") %>%
  left_join(genes, by = "gene_id") %>%
  select(mouse_id, mouse_line, mouse_line_group, sex, age, gene, gene_symbol, value) %>%
  arrange(age) %>%
  mutate(age = as_factor(age))

# Generate sample of data to iterate with ----
# Sample by gene, and sampling by % of zeros to get a good idea of what plots will look like

set.seed(1234)

sample_genes <- gene_expressions %>%
  mutate(zero = value == 0) %>%
  group_by(gene) %>%
  summarise(prop_zero = mean(zero)) %>%
  mutate(prop_zero_group = cut(prop_zero, breaks = seq(0, 1, 0.25), include.lowest = TRUE)) %>%
  group_by(prop_zero_group) %>%
  sample_n(5) %>%
  pull(gene)

gene_expressions <- gene_expressions %>%
  filter(gene %in% sample_genes)

usethis::use_data(gene_expressions, overwrite = TRUE)
