library(dplyr)
library(arrow)
library(stringr)
library(purrr)

# Read in gene_expressions RDA file and partition/save to parquet
# This step should be run on the server - the parquet files are ignored by github

load(here::here("inst", "extdata", "gene_expressions.rda"))

# Save parquet, partitioned by the first letter of the gene

gene_expressions <- gene_expressions %>%
  mutate(partition = tolower(str_sub(gene, 1, 1)))

walk(
  unique(gene_expressions[["partition"]]),
  function(x) {
    fs::dir_create(here::here("inst", "extdata", "gene_expressions", paste0("partition=", x)))
    gene_expressions %>%
      filter(partition == x) %>%
      write_parquet(here::here("inst", "extdata", "gene_expressions", paste0("partition=", x), paste0(x, ".parquet")), compression = "uncompressed")
  }
)
