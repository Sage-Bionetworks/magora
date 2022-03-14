library(vdiffr)
context("magora_boxplot") # Required by vdiffr still

phenotype_mouse_models <- levels(phenotypes[["mouse_model"]])

test_that("magora_boxplot produces box plots comparing the phenotype by mouse model, age, and sex", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Astrocyte Cell Density (GFAP)",
      .data$mouse_model %in% phenotype_mouse_models,
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("faceted-box-plots", p)
})

test_that("magora_boxplot produces two rows of facets when 3-4 mouse models are selected", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque Size (Thio-S)",
      .data$mouse_model %in% phenotype_mouse_models[1:3],
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models[1:3]) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("two-row-facets-three", p)

  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque Size (Thio-S)",
      .data$mouse_model %in% phenotype_mouse_models,
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("two-row-facets-four", p)
})

test_that("magora_boxplot adds text to any facet without data", {
  p <- phenotypes %>%
    dplyr::filter(mouse_model != phenotype_mouse_models[[2]]) %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia Cell Density (IBA1)",
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-experiment", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_model != phenotype_mouse_models[[1]]) %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia Cell Density (IBA1)",
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-control", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_model %in% phenotype_mouse_models[1:2]) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque Size (Thio-S)",
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-interaction", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_model %in% phenotype_mouse_models[1:2]) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque Size (Thio-S)",
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-both", p)
})

test_that("All levels of age are shown in the plot even if not present in the filtered data", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Astrocyte Cell Density (GFAP)",
      .data$mouse_model %in% phenotype_mouse_models,
      .data$tissue %in% "Hippocampus"
    ) %>%
    expand_mouse_model_factor_from_selection(phenotype_mouse_models) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("not-all-ages", p)
})

test_that("magora_boxplot shows facets in the order selected", {
  p <- phenotypes %>%
    dplyr::filter(
      phenotype %in% "Astrocyte Cell Density (GFAP)",
      mouse_model %in% phenotype_mouse_models
    ) %>%
    expand_mouse_model_factor_from_selection(rev(phenotype_mouse_models)) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("pathology-different-order", p)
})
