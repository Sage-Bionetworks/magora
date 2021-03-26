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

syn_ids <- syn_ids %>%
  filter(id %in% c(
    "syn24847795", "syn24847793", "syn24847794", "syn24847798",
    "syn24847796", "syn24847797", "syn25175556", "syn25175557", "syn25175558",
    "syn25175559", "syn25175552", "syn25175553", "syn25175554", "syn25175555",
    "syn25175564", "syn25175565", "syn25175566", "syn25175567",
    "syn25175560", "syn25175561", "syn25175562", "syn25175563",
    "syn25312653", "syn25312654", "syn25312655", "syn25312656",
    "syn25312651", "syn25312652", "syn25312659", "syn25312660", "syn25312661",
    "syn25312662", "syn25312657", "syn25312658", "syn25312854", "syn25312853",
    "syn25312856", "syn25312855"
  )) %>%
  mutate(version = case_when(
    id %in% c(
      "syn25175556", "syn25175557", "syn25175558", "syn25175559",
      "syn25175552", "syn25175553", "syn25175554", "syn25175555", "syn25175564",
      "syn25175565", "syn25175566", "syn25175567", "syn25175560",
      "syn25175561", "syn25175562", "syn25175563"
    ) ~ 2,
    id %in% c(
      "syn24847795", "syn24847793", "syn24847794", "syn24847798",
      "syn24847796", "syn24847797", "syn25170166"
    ) ~ 2,
    id %in% c("syn25312653", "syn25312654", "syn25312655", "syn25312656",
      "syn25312651", "syn25312652", "syn25312659", "syn25312660", "syn25312661",
      "syn25312662", "syn25312657", "syn25312658", "syn25312854", "syn25312853",
      "syn25312856", "syn25312855") ~ 1
  ))

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

gene_expressions_for_volcano <- map2(syn_ids[["id"]], syn_ids[["name"]], ~ read_csv(here::here("data-raw", "gene_expressions", .x, .y), col_types = "ccdccddd"))
names(gene_expressions_for_volcano) <- syn_ids[["id"]]

gene_expressions_for_volcano <- gene_expressions_for_volcano %>%
  map(rename_all, tolower) %>%
  bind_rows(.id = "syn_id")

# Clean data ----

gene_expressions_for_volcano <- gene_expressions_for_volcano %>%
  rename(gene_id = geneid)

gene_expressions_for_volcano <- gene_expressions_for_volcano %>%
  mutate(tissue = str_to_title(tissue))

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
  left_join(genes, by = "gene_id") %>%
  select(-gene_symbol, -gene_id)

gene_expressions_for_volcano <- gene_expressions_for_volcano %>%
  select(-padj, -syn_id)

usethis::use_data(gene_expressions_for_volcano, overwrite = TRUE)

# Separately save tissue available for each strain, for easily changing inputs available

gene_expressions_tissue <- split(gene_expressions_for_volcano, gene_expressions_for_volcano$strain) %>%
  map(function(x) distinct(x, tissue) %>% pull(tissue))

usethis::use_data(gene_expressions_tissue, overwrite = TRUE)
