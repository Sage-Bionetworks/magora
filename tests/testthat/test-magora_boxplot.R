library(vdiffr)
context("magora_boxplot") # Required by vdiffr still

pathology_mouse_models <- levels(dplyr::bind_rows(pathology)[["mouse_model"]])
pathology_mouse_model_groups <- names(pathology)

test_that("magora_boxplot produces box plots comparing the phenotype by mouse model, age, and sex", {
  p <- pathology %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          .data$phenotype %in% "Astrocyte Cell Density (GFAP)",
          .data$mouse_model %in% pathology_mouse_models,
          .data$tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups, use_theme_sage = FALSE)
  expect_doppelganger("faceted-box-plots", p)
})

test_that("magora_boxplot produces two rows of facets when 3-4 mouse models are selected", {
  p <- pathology[pathology_mouse_model_groups[1:2]] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          .data$phenotype %in% "Dystrophic Neurites (LAMP1)",
          .data$mouse_model %in% pathology_mouse_models[1:3],
          .data$tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups[1:2], use_theme_sage = FALSE)
  expect_doppelganger("two-row-facets-three", p)

  p <- pathology[pathology_mouse_model_groups[1:2]] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          .data$phenotype %in% "Dystrophic Neurites (LAMP1)",
          .data$mouse_model %in% pathology_mouse_models,
          .data$tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups[1:2], use_theme_sage = FALSE)
  expect_doppelganger("two-row-facets-four", p)
})

test_that("magora_boxplot adds text to any facet without data", {
  p <- pathology[pathology_mouse_model_groups[1]] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(mouse_model != pathology_mouse_models[[2]]) %>%
        dplyr::filter(
          .data$phenotype %in% "Microglia Cell Density (IBA1)",
          .data$tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups[1], use_theme_sage = FALSE)
  expect_doppelganger("no-data-experiment", p)

  p <- pathology[pathology_mouse_model_groups[1]] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(mouse_model != pathology_mouse_models[[1]]) %>%
        dplyr::filter(
          .data$phenotype %in% "Microglia Cell Density (IBA1)",
          .data$tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups[1], use_theme_sage = FALSE)
  expect_doppelganger("no-data-control", p)

  p <- pathology[pathology_mouse_model_groups[1:2]] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(mouse_model %in% pathology_mouse_models[1:2]) %>%
        dplyr::filter(
          .data$phenotype %in% "Plaque Size (Thio-S)",
          .data$tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups[1:2], use_theme_sage = FALSE)
  expect_doppelganger("no-data-both", p)
})

test_that("All levels of age are shown in the plot even if not present in the filtered data", {
  p <- pathology %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          .data$phenotype %in% "Astrocyte Cell Density (GFAP)",
          .data$mouse_model %in% pathology_mouse_models,
          .data$tissue %in% "Hippocampus"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups, use_theme_sage = FALSE)
  expect_doppelganger("not-all-ages", p)
})

test_that("magora_boxplot shows facets in the order selected", {
  p <- pathology %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          phenotype %in% "Astrocyte Cell Density (GFAP)",
          mouse_model %in% pathology_mouse_models
        )
    }) %>%
    magora_boxplot(rev(pathology_mouse_model_groups), use_theme_sage = FALSE)
  expect_doppelganger("pathology-different-order", p)
})

test_that("magora_boxplot has different y axes across rows, but shared within a row", {
  p <- pathology %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          phenotype %in% "Astrocyte Cell Density (GFAP)"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups, use_theme_sage = FALSE)
  expect_doppelganger("pathology-y-axes-scaled-by-model", p)
})

test_that("magora_boxplot y-axes are shared within a row even if one facet is missing data", {
  p <- pathology %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          phenotype %in% "Dystrophic Neurites (LAMP1)"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups, use_theme_sage = FALSE)
  expect_doppelganger("pathology-y-axes-scaled-by-model-facet-inherit-from-row-if-missing-data", p)
})

test_that("magora_boxplot have no axes showing if both model/control are missing data", {
  p <- pathology[pathology_mouse_model_groups[3]] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          phenotype %in% "phospho-tau (AT8)"
        )
    }) %>%
    magora_boxplot(pathology_mouse_model_groups[3], use_theme_sage = FALSE)
  expect_doppelganger("pathology-y-axes-scaled-by-model-facet-inherit-from-plot-if-missing-data", p)
})

test_that("magora_boxplot panels with no data has same appearance as panels with data", {
  p <- pathology[c("5xFAD", "Trem2-R47H_NSS")] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          phenotype %in% "Astrocyte Cell Density (GFAP)"
        )
    }) %>%
    magora_boxplot(c("5xFAD", "Trem2-R47H_NSS"), use_theme_sage = FALSE)

  expect_doppelganger("pathology-panel-no-data-matches-panel-with-data", p)
})

test_that("magora_boxplot with all 0s have y axis go from 0 to 10", {

  p <- pathology["3xTg-AD"] %>%
    purrr::map(function(x) {
      x %>%
        dplyr::filter(
          phenotype %in% "Plaque Size (Thio-S)" &
            tissue %in% "Cerebral Cortex"
        )
    }) %>%
    magora_boxplot("3xTg-AD", use_theme_sage = FALSE)

  expect_doppelganger("pathology-all-zeros", p)
})
