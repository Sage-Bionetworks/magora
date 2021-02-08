library(AnnotationDbi)
library(EnsDb.Mmusculus.v79)
library(janitor)
library(readr)
library(dplyr)

# Read data ----

gene_expressions_for_volcano <- read_csv(here::here("data-raw", "gene_expressions", "5XFAD_Male_4M.csv"))

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

EnhancedVolcano(gene_expressions_for_volcano, lab = rownames(gene_expressions_for_volcano), x = "log2fold_change", y = "padj", pCutoff = 0.05, FCcutoff = 1)

usethis::use_data(gene_expressions_for_volcano, overwrite = TRUE)
