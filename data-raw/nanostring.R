library(dplyr)
library(limma)
library(readr)
library(janitor)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(synapser)

# Get data -----

# synLogin()

## Nanostring ----

# synGet("syn22105392", version = 1, downloadLocation = here::here("data-raw", "nanostring"))

nanostring_raw <- read_csv(here::here("data-raw", "nanostring", "logHKNormalized_Allsamples.csv"))

## Nanostring metadata ----

# Get biospecimen metadata only because it contains the specimen ID (which the nanostring data uses) and individual ID
# The age/sex are then in the individual metadata, which only has individual ID
# So we need the biospecimen metadata to get the ID

# synGet("syn22107820", version = 4, downloadLocation = here::here("data-raw", "nanostring"))

nanostring_biospecimen_metadata_raw <- read_csv(here::here("data-raw", "nanostring", "Jax.IU.Pitt_PrimaryScreen_biospecimen_metadata.csv"))

# synGet("syn22107818", version = 3, downloadLocation = here::here("data-raw", "nanostring"))

nanostring_individual_metadata_raw <- read_csv(here::here("data-raw", "nanostring", "Jax.IU.Pitt_PrimaryScreen_individual_metadata.csv")) %>%
  remove_empty(c("rows", "cols"))

## AMPA-D Modules -----

# synGet("syn21483261", version = 1, downloadLocation = here::here("data-raw", "nanostring"))

load(here::here("data-raw", "nanostring", "aggregateModules.rda"))

modules <- c("cbe_mods", "dlpfc_mods", "fp_mods", "ifg_mods", "phg_mods", "stg_mods", "tcx_mods")

ampad_modules_raw <- map_dfr(modules, ~ get(.x)[["df"]]) %>%
  as_tibble()

rm(list = modules)

## AMPA-D Modules logFC (human data) ----

# synGet("syn14237651", version = 1, downloadLocation = here::here("data-raw", "nanostring"))

ampad_fc_raw <- read.table(file = here::here("data-raw", "nanostring", "differentialExpressionSummary.tsv"), sep = "\t", header = TRUE)

# Clean data ----

## Nanostring ----

# Convert nanostring data to long, replace "." in specimen_id with "-" to match metadata formatting

nanostring <- nanostring_raw %>%
  pivot_longer(cols = -X1, names_to = "specimen_id", values_to = "value") %>%
  rename(gene = X1) %>%
  mutate(specimen_id = str_replace(specimen_id, "\\.", "-"))

# Get individual ID from biospecimen metadata

nanostring_with_id <- nanostring %>%
  left_join(nanostring_biospecimen_metadata_raw, by = c("specimen_id" = "specimenID")) %>%
  select(gene, individual_id = individualID, specimen_id, value)

# Ensure no rows were added

nrow(nanostring_with_id) == nrow(nanostring)

# Ensure all records have individual_id

nanostring_with_id %>%
  filter(is.na(individual_id)) %>%
  nrow() == 0

## Nanostring individual metadata - calculate age from birth and death ----

nanostring_individual_metadata <- nanostring_individual_metadata_raw %>%
  mutate(
    across(c(dateBirth, dateDeath), mdy),
    age_interval = interval(dateBirth, dateDeath),
    age = round(age_interval / months(1))
  ) %>%
  select(individual_id = individualID, genotype, sex, age)

## Combine nanostring data with individual metadata ----

nanostring_with_metadata <- nanostring_with_id %>%
  left_join(nanostring_individual_metadata,
    by = "individual_id"
  )

# Split nanostring data by model and variant/control

nanostring_with_metadata <- nanostring_with_metadata %>%
  separate(genotype, into = c("model", "type")) %>%
  mutate(
    model = toupper(model),
    type = case_when(
      type == "NONCARRIER" ~ "control",
      type == "HOMOZYGOUS" ~ "variant"
    )
  )

## AMPA-D Modules ----

ampad_modules <- ampad_modules_raw %>%
  distinct(tissue = brainRegion, module = Module, gene = external_gene_name)

## AMPA-D Modules logFC ----

# Filter out sex "all" to match on sex
ampad_fc <- ampad_fc_raw %>%
  as_tibble() %>%
  select(tissue = Tissue, gene = hgnc_symbol, sex = Sex, ampad_fc = logFC) %>%
  mutate(sex = tolower(sex)) %>%
  filter(sex != "all")

# Combine with modules so correlation can be done per module
ampad_modules_fc <- ampad_modules %>%
  inner_join(ampad_fc, by = c("gene", "tissue"))

# Differential expression analysis ----

# DEA compares each mouse model to its control (with the same sex and age)

# Separate out variants and controls, rename and keep relevant columns only

widen_and_nest_samples <- function(data) {
  data %>%
    select(gene, specimen_id, value, sex, age, model, type) %>%
    pivot_wider(names_from = specimen_id, values_from = value, names_prefix = "value_") %>%
    group_by(sex, age, model, type) %>%
    nest() %>%
    ungroup() %>%
    mutate(data = map(data, ~ remove_empty(.x, "cols")))
}

ns_control <- nanostring_with_metadata %>%
  filter(type == "control") %>%
  widen_and_nest_samples()

ns_variant <- nanostring_with_metadata %>%
  filter(type == "variant") %>%
  widen_and_nest_samples()

# Only iterate over ns_variant that actually have a control

# Summarise what combinations are available

ns_variant %>%
  select(model, sex, age, variant = type) %>%
  full_join(ns_control %>%
    select(model, sex, age, control = type)) %>%
  arrange(model, sex, age)

ns_variant_control <- ns_variant %>%
  inner_join(ns_control, by = c("sex", "age", "model"), suffix = c("_variant", "_control")) %>%
  select(model, sex, age, data_variant, data_control)

# For each group (model, sex, age), do the differential expression analysis with comparisons to the appropriate control

differential_expression_analysis <- function(data_variant, data_control) {
  data_variant_control <- data_variant %>%
    left_join(data_control, by = "gene")

  n_variant <- ncol(data_variant) - 1

  n_control <- ncol(data_control) - 1

  group <- factor(c(rep("Variant", n_variant), rep("Control", n_control)))
  design <- model.matrix(~ 0 + group)
  contrast <- makeContrasts(groupVariant - groupControl, levels = design)

  fit <- lmFit(
    data_variant_control %>%
      select(-gene),
    design
  )
  fit_contrasts <- contrasts.fit(fit, contrast)
  fit_contrasts_ebayes <- eBayes(fit_contrasts)
  log_fc <- topTable(fit_contrasts_ebayes, n = Inf, sort = "none")["logFC"]

  data_variant_control %>%
    bind_cols(log_fc) %>%
    select(gene, ns_fc = logFC)
}

# Will fail on cases where there's not repeated samples: https://support.bioconductor.org/p/59168/
# Run function with possibly()

possible_differential_expression_analysis <- possibly(differential_expression_analysis, otherwise = tibble(gene = NA, ns_fc = NA))

ns_fc <- ns_variant_control %>%
  mutate(dea_res = map2(data_variant, data_control, possible_differential_expression_analysis)) %>%
  select(-data_variant, -data_control) %>%
  unnest(dea_res) %>%
  filter(!(is.na(gene) & is.na(ns_fc)))

# Correlation between log_fc of mouse models and AMPAD modules -----

# Calculate correlation and p-value, joining by gene and sex, for each module, model, sex, and age

ns_vs_ampad_fc <- ns_fc %>%
  mutate(gene = toupper(gene)) %>%
  inner_join(ampad_modules_fc, by = c("gene", "sex")) %>%
  select(module, model, sex, age, gene, ns_fc, ampad_fc) %>%
  group_by(module, model, sex, age) %>%
  nest() %>%
  mutate(
    cor_test = map(data, ~ cor.test(.x[["ns_fc"]], .x[["ampad_fc"]], method = "pearson")),
    estimate = map_dbl(cor_test, "estimate"),
    p_value = map_dbl(cor_test, "p.value")
  ) %>%
  ungroup() %>%
  select(-cor_test)

# Process for plotting ----

# Filter significant results only, adjust format
nanostring_significant <- ns_vs_ampad_fc %>%
  select(module, model, sex, age, estimate, p_value) %>%
  filter(p_value <= 0.05) %>%
  select(module, model, sex, age, estimate, p_value) %>%
  mutate(model_new = glue::glue("{model}_{sex}_{age}")) %>%
  select(module, model_new, estimate) %>%
  pivot_wider(names_from = module, values_from = estimate, values_fill = 0)

# Convert to matrix for plotting
nanostring <- as.matrix(nanostring_significant[ , -1])
rownames(nanostring) <- nanostring_significant[["model_new"]]

# Save data ----

usethis::use_data(nanostring, overwrite = TRUE)
