library(dplyr)
library(limma)
library(readr)
library(janitor)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(forcats)
library(synapser)

synLogin()

source(here::here("data-raw", "check_latest_version.R"))

# Get data -----

## Nanostring ----

nanostring_id <- "syn22105392"
nanostring_version <- 2

check_latest_version(nanostring_id, nanostring_version)

synGet(nanostring_id, version = nanostring_version, downloadLocation = here::here("data-raw", "nanostring"), ifcollision = "overwrite.local")

nanostring_raw <- read_csv(here::here("data-raw", "nanostring", "logHKNormalized_Allsamples.csv"))

## Nanostring metadata ----

# Get biospecimen metadata only because it contains the specimen ID (which the nanostring data uses) and individual ID
# The age/sex are then in the individual metadata, which only has individual ID
# So we need the biospecimen metadata to get the ID

biospecimen_id <- "syn22107820"
biospecimen_version <- 6

check_latest_version(biospecimen_id, biospecimen_version)

synGet(biospecimen_id, version = biospecimen_version, downloadLocation = here::here("data-raw", "nanostring"), ifcollision = "overwrite.local")

nanostring_biospecimen_metadata_raw <- read_csv(here::here("data-raw", "nanostring", "Jax.IU.Pitt_PrimaryScreen_biospecimen_metadata.csv"))

individual_id <- "syn22107818"
individual_version <- 7

check_latest_version(individual_id, individual_version)

synGet(individual_id, version = individual_version, downloadLocation = here::here("data-raw", "nanostring"), ifcollision = "overwrite.local")

nanostring_individual_metadata_raw <- read_csv(here::here("data-raw", "nanostring", "Jax.IU.Pitt_PrimaryScreen_individual_metadata.csv")) %>%
  remove_empty(c("rows", "cols"))

## AMP-AD Modules -----

ampad_modules_id <- "syn21483261"
ampad_modules_version <- 1

check_latest_version(ampad_modules_id, ampad_modules_version)

synGet(ampad_modules_id, version = ampad_modules_version, downloadLocation = here::here("data-raw", "nanostring"))

load(here::here("data-raw", "nanostring", "aggregateModules.rda"))

modules <- c("cbe_mods", "dlpfc_mods", "fp_mods", "ifg_mods", "phg_mods", "stg_mods", "tcx_mods")

ampad_modules_raw <- map_dfr(modules, ~ get(.x)[["df"]]) %>%
  as_tibble()

rm(list = modules)

## Module clusters ----

# From the paper: https://molecularneurodegeneration.biomedcentral.com/articles/10.1186/s13024-020-00412-5

cluster_a <- tibble(
  module = c("TCXblue", "PHGyellow", "IFGyellow"),
  cluster = "Consensus Cluster A (ECM organization)",
  cluster_label = "Consensus\nCluster\nA (ECM\norganization)"
)

cluster_b <- tibble(
  module = c("DLPFCblue", "CBEturquoise", "STGblue", "PHGturquoise", "IFGturquoise", "TCXturquoise", "FPturquoise"),
  cluster = "Consensus Cluster B (Immune system)",
  cluster_label = "Consensus Cluster B\n(Immune system)"
)

cluster_c <- tibble(
  module = c("IFGbrown", "STGbrown", "DLPFCyellow", "TCXgreen", "FPyellow", "CBEyellow", "PHGbrown"),
  cluster = "Consensus Cluster C (Neuronal system)",
  cluster_label = "Consensus Cluster C\n(Neuronal system)"
)

cluster_d <- tibble(
  module = c("DLPFCbrown", "STGyellow", "PHGgreen", "CBEbrown", "TCXyellow", "IFGblue", "FPblue"),
  cluster = "Consensus Cluster D (Cell Cycle, NMD)",
  cluster_label = "Consensus Cluster D\n(Cell Cycle, NMD)"
)

cluster_e <- tibble(
  module = c("FPbrown", "CBEblue", "DLPFCturquoise", "TCXbrown", "STGturquoise", "PHGblue"),
  cluster = "Consensus Cluster E (Organelle Biogensis, Cellular stress response)",
  cluster_label = "Consensus Cluster\nE(Organelle\nBiogensis,\nCellular stress\nresponse)"
)

module_clusters <- cluster_a %>%
  bind_rows(cluster_b) %>%
  bind_rows(cluster_c) %>%
  bind_rows(cluster_d) %>%
  bind_rows(cluster_e) %>%
  mutate(cluster_label = fct_inorder(cluster_label))

## AMP-AD Modules logFC (human data) ----

ampad_logfc_id <- "syn14237651"
ampad_logfc_version <- 1

check_latest_version(ampad_logfc_id, ampad_logfc_version)

synGet(ampad_logfc_id, version = ampad_logfc_version, downloadLocation = here::here("data-raw", "nanostring"))

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

# This is not the case - some individuals are missing from the biospecimen metadata
# This is fine - some are missing but we won't use them
# Relevant issue: https://github.com/Sage-Bionetworks/magora/issues/62

nanostring_with_id <- nanostring_with_id %>%
  filter(!is.na(individual_id))

## Nanostring individual metadata ----

# Only include mice with NA treatmentType

nanostring_individual_metadata <- nanostring_individual_metadata_raw %>%
  filter(is.na(treatmentType))

# AgeDeath and individualCommonGenotype contain the age and model, so we don't need to derive those anymore

nanostring_individual_metadata <- nanostring_individual_metadata %>%
  mutate(
    sex = str_to_title(sex)
  ) %>%
  select(individual_id = individualID, sex, age = ageDeath, mouse_model = individualCommonGenotype)

## Combine nanostring data with individual metadata ----

nanostring_with_metadata <- nanostring_with_id %>%
  inner_join(nanostring_individual_metadata,
    by = "individual_id"
  )

# Create age groups

nanostring_with_metadata <- nanostring_with_metadata %>%
  mutate(age_group = case_when(
    age %in% 2:5 ~ "2 - 5 Months",
    age %in% 6:9 ~ "6 - 9 Months",
    age %in% 10:14 ~ "10 - 14 Months"
  ))

## AMP-AD Modules ----

ampad_modules <- ampad_modules_raw %>%
  distinct(tissue = brainRegion, module = Module, gene = external_gene_name)

## AMP-AD Modules logFC ----

# Filter only to include those with Model = "Diagnosis", and Comparison = "AD-CONTROL"
# These have sex "all" - not joining by sex, just comparing to all

ampad_fc <- ampad_fc_raw %>%
  as_tibble() %>%
  filter(Model == "Diagnosis", Comparison == "AD-CONTROL") %>%
  select(tissue = Tissue, gene = hgnc_symbol, ampad_fc = logFC) %>%
  filter(gene != "")

# Combine with modules so correlation can be done per module
ampad_modules_fc <- ampad_modules %>%
  inner_join(ampad_fc, by = c("gene", "tissue"))

# Double check that there is just one value for each tissue and module (for paired sampling)

ampad_modules_fc %>%
  count(module, gene) %>%
  count(n)

# All looks good!

# Differential expression analysis ----

# DEA compares each mouse model to ALL controls (with the same sex and age)

# Separate out variants and controls, rename and keep relevant columns only
# Control is C57BL6J

ns_control <- nanostring_with_metadata %>%
  filter(mouse_model == "C57BL6J") %>%
  select(gene, specimen_id, value, sex, age_group, mouse_model) %>%
  pivot_wider(names_from = specimen_id, values_from = value, names_prefix = "value_") %>%
  group_by(sex, age_group, mouse_model) %>%
  nest() %>%
  ungroup() %>%
  mutate(data = map(data, ~ remove_empty(.x, "cols")))

ns_variant <- nanostring_with_metadata %>%
  filter(mouse_model != "C57BL6J") %>%
  select(gene, specimen_id, value, sex, age_group, mouse_model) %>%
  pivot_wider(names_from = specimen_id, values_from = value, names_prefix = "value_") %>%
  group_by(sex, age_group, mouse_model) %>%
  nest() %>%
  ungroup() %>%
  mutate(data = map(data, ~ remove_empty(.x, "cols")))

# Only iterate over ns_variant that actually have a control - since not joining by model, just ensuring that each variant has controls of the same age group / sex

# Summarise what combinations are available

ns_variant %>%
  distinct(sex, age_group, variant = mouse_model) %>%
  full_join(ns_control %>%
    select(sex, age_group, control = mouse_model), by = c("sex", "age_group")) %>%
  arrange(sex, age_group)

ns_variant_control <- ns_variant %>%
  inner_join(ns_control, by = c("sex", "age_group"), suffix = c("_variant", "_control")) %>%
  select(mouse_model = mouse_model_variant, sex, age_group, data_variant, data_control)

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
  # eBayes is not necessary - just shrinks the standard errors, but does not affect logFC
  # Also fails when there is not more than one sample, which we have some of
  # The coefficients are identical with or without eBayes - so skip this step
  # fit_contrasts_ebayes <- eBayes(fit_contrasts)
  # log_fc <- topTable(fit_contrasts_ebayes, n = Inf, sort = "none")["logFC"]

  log_fc <- fit_contrasts$coefficients[, 1]

  data_variant_control %>%
    mutate(ns_fc = log_fc) %>%
    select(gene, ns_fc)
}

ns_fc <- ns_variant_control %>%
  mutate(dea_res = map2(data_variant, data_control, differential_expression_analysis)) %>%
  select(-data_variant, -data_control) %>%
  unnest(dea_res) %>%
  filter(!(is.na(gene) & is.na(ns_fc)))

# What combinations were lost?

ns_fc_models <- ns_fc %>%
  distinct(mouse_model, sex, age_group)

ns_variant_control %>%
  anti_join(ns_fc_models, by = c("mouse_model", "sex", "age_group"))

# None!

# Correlation between log_fc of mouse models and AMP-AD modules -----

# Calculate correlation and p-value, joining by gene (not sex), for each module, model, sex, and age group
# These are paired properly because there is only one value from the nanostring data for each model, sex, and age group (the log fold change), AND only one value from the AMP-AD data for each module

ns_vs_ampad_fc <- ns_fc %>%
  mutate(gene = toupper(gene)) %>%
  inner_join(ampad_modules_fc, by = "gene") %>%
  select(module, mouse_model, sex, age_group, gene, ns_fc, ampad_fc) %>%
  group_by(module, mouse_model, sex, age_group) %>%
  nest(data = c(gene, ns_fc, ampad_fc)) %>%
  mutate(
    cor_test = map(data, ~ cor.test(.x[["ns_fc"]], .x[["ampad_fc"]], method = "pearson")),
    estimate = map_dbl(cor_test, "estimate"),
    p_value = map_dbl(cor_test, "p.value")
  ) %>%
  ungroup() %>%
  select(-cor_test)

# Process data for plotting ----

# Flag for significant results, add cluster information to modules

nanostring <- ns_vs_ampad_fc %>%
  mutate(significant = p_value < 0.05) %>%
  left_join(module_clusters, by = "module") %>%
  select(cluster, cluster_label, module, mouse_model, sex, age_group, correlation = estimate, p_value, significant)

# Create a version of the data for plotting - clean up naming, order factors, etc

nanostring_for_plot <- nanostring %>%
  arrange(cluster) %>%
  mutate(
    module = fct_inorder(module),
    model_sex = glue::glue("{mouse_model} ({sex})"),
  ) %>%
  arrange(model_sex) %>%
  mutate(
    model_sex = fct_inorder(model_sex),
    model_sex = fct_rev(model_sex),
  ) %>%
  separate(age_group, into = "min_age", sep = " -", remove = FALSE, convert = TRUE, extra = "drop") %>%
  mutate(age_group = fct_reorder(age_group, min_age))

nanostring <- nanostring %>%
  select(-cluster_label)

# Save data ----

usethis::use_data(nanostring, overwrite = TRUE)
usethis::use_data(nanostring_for_plot, overwrite = TRUE)
