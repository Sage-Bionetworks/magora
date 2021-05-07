library(AnnotationDbi)
library(EnsDb.Mmusculus.v79)
library(janitor)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(synapser)
library(forcats)
library(stringr)

synLogin()

# Manually filtering files in order to keep track of changes - once data is more stable
# can just look for children of syn24243915

syn_get_children <- function(syn_id) {
  synGetChildren(syn_id) %>%
    as.list() %>%
    transpose() %>%
    as_tibble() %>%
    select(id, name, latest_version = versionNumber, type) %>%
    unnest(cols = c(id, name, latest_version, type))
}

syn_ids_initial <- syn_get_children("syn24243915")

syn_ids_children <- syn_ids_initial %>%
  filter(type == "org.sagebionetworks.repo.model.Folder") %>%
  pull(id) %>%
  map_dfr(syn_get_children)

# Confirm there are no more subfolders

syn_ids_children %>%
  filter(type == "org.sagebionetworks.repo.model.Folder") %>%
  nrow() == 0

syn_ids <- syn_ids_initial %>%
  filter(type != "org.sagebionetworks.repo.model.Folder") %>%
  bind_rows(syn_ids_children) %>%
  select(id, name, latest_version)

# Read in IDs actually used and their versions - for easier keeping track of changes

syn_ids_using <- read_csv(here::here("data-raw", "gene_expressions", "data_sources.csv"), col_types = "cd")

# Check that no files are being missed, except for "test.csv"

syn_ids %>%
  anti_join(syn_ids_using, by = "id") %>%
  filter(name != "test.csv") %>%
  nrow() == 0

syn_ids <- syn_ids %>%
  inner_join(syn_ids_using, by = "id")

# Check that version used is latest version
syn_ids %>%
  filter(latest_version != version) %>%
  nrow() == 0

syn_ids <- syn_ids %>%
  select(-latest_version)

# Download all

# pwalk(syn_ids, function(id, name, version) {
#   synGet(id, version = version, ifcollision = "overwrite.local", downloadLocation = here::here("data-raw", "gene_expressions", id))
# })

# Read in and combine

gene_expressions <- map2(syn_ids[["id"]], syn_ids[["name"]], ~ read_csv(here::here("data-raw", "gene_expressions", .x, .y), col_types = "ccdccddd"))
names(gene_expressions) <- syn_ids[["id"]]

gene_expressions <- gene_expressions %>%
  map(rename_all, tolower) %>%
  bind_rows(.id = "syn_id")

# Clean data ----

gene_expressions <- gene_expressions %>%
  rename(gene_id = geneid)

gene_expressions <- gene_expressions %>%
  mutate(tissue = str_to_title(tissue))

# Query gene symbol to use in place of ID ----

genes <- gene_expressions %>%
  distinct(gene_id)

gene_symbols <- AnnotationDbi::select(EnsDb.Mmusculus.v79, keys = genes[["gene_id"]], columns = "SYMBOL", keytype = "GENEID") %>%
  as_tibble() %>%
  dplyr::rename(gene_id = GENEID, gene_symbol = SYMBOL) %>%
  mutate(gene_symbol = ifelse(gene_symbol %in% c(""), NA_character_, gene_symbol))

# Only use the symbol if it exists for one id only (i.e. mapping is 1-1)

gene_symbols <- gene_symbols %>%
  add_count(gene_symbol) %>%
  filter(n == 1) %>%
  select(-n)

# If the symbol exists, use that - otherwise, use gene id

genes <- genes %>%
  left_join(gene_symbols, by = "gene_id") %>%
  mutate(gene = coalesce(gene_symbol, gene_id))

gene_expressions <- gene_expressions %>%
  left_join(genes, by = "gene_id") %>%
  select(-gene_symbol, -gene_id, -syn_id)

# Check that values are unique

gene_expressions %>%
  count(strain, tissue, sex, age, gene) %>%
  filter(n > 1) %>%
  nrow() == 0

# Round fold change and p-values to a reasonable amount ----

gene_expressions <- gene_expressions %>%
  mutate(across(c(log2foldchange, pvalue, padj), ~ round(.x, 5)))

# Flag as significant for plotting ----

gene_expressions <- gene_expressions %>%
  dplyr::mutate(diff_expressed = dplyr::case_when(
    log2foldchange > 1 & padj < 0.05 ~ "Upregulated",
    log2foldchange < -1 & padj < 0.05 ~ "Downregulated",
    is.na(log2foldchange) | is.na(padj) ~ NA_character_,
    TRUE ~ "Not Significant"
  ))

# Generate labels - only genes that are upregulated/downregulated, and not super long names

gene_expressions_labels <- gene_expressions %>%
  dplyr::filter(
    diff_expressed != "Not Significant",
    nchar(gene) < 18
  ) %>%
  dplyr::mutate(label = gene)

usethis::use_data(gene_expressions_labels, overwrite = TRUE)
usethis::use_data(gene_expressions, overwrite = TRUE)

# Separately save tissue available for each strain, for easily changing inputs available

gene_expressions_tissue <- split(gene_expressions, gene_expressions$strain) %>%
  map(function(x) distinct(x, tissue) %>% pull(tissue))

usethis::use_data(gene_expressions_tissue, overwrite = TRUE)
