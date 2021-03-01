library(AnnotationDbi)
library(EnsDb.Mmusculus.v79)
library(janitor)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(synapser)

synLogin()

# Read data ----

# Get IDs of all data in folder syn24243915
syn_ids <- synGetChildren("syn24243915") %>%
  as.list() %>%
  transpose() %>%
  as_tibble() %>%
  select(id, name) %>%
  unnest(cols = c(id, name)) %>%
  mutate(version = 1) # Using version 1 for all - will update if new versions are used

# Download all

pmap(syn_ids, function(id, name, version) {
  synGet(id, version = version, ifcollision = "overwrite.local", downloadLocation = here::here("data-raw", "gene_expressions", "volcano"))
})

# Read in and combine

gene_expressions_for_volcano <- map_dfr(syn_ids[["name"]], ~ read_csv(here::here("data-raw", "gene_expressions", "volcano", .x), col_types = "dccdcddd"))

# Clean data ----

gene_expressions_for_volcano <- gene_expressions_for_volcano %>%
  clean_names() %>%
  select(-x1)

# Query gene symbol to use in place of ID ----

genes <- gene_expressions_for_volcano %>%
  distinct(gene_id)

gene_symbols <- AnnotationDbi::select(EnsDb.Mmusculus.v79, keys = genes[["gene_id"]], columns = "SYMBOL", keytype = "GENEID") %>%
  as_tibble() %>%
  dplyr::rename(gene_id = GENEID, gene_symbol = SYMBOL) %>%
  mutate(gene_symbol = ifelse(gene_symbol %in% c(""), NA_character_, gene_symbol))

# If the symbol exists, use that - otherwise, use gene id

genes <- genes %>%
  left_join(gene_symbols, by = "gene_id") %>%
  mutate(gene = coalesce(gene_symbol, gene_id))

gene_expressions_for_volcano <- gene_expressions_for_volcano %>%
  left_join(genes, by = "gene_id")

usethis::use_data(gene_expressions_for_volcano, overwrite = TRUE)
