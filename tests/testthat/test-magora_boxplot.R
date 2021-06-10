library(vdiffr)
context("magora_boxplot") # Required by vdiffr still

# Set up fake phenotypes data for testing on

set.seed(1234)

phenotypes_df <- expand.grid(
  tissue = c("Cortex", "Hippocampus"),
  sex = c("Male", "Female"),
  mouse_model_group = c("BL6", "BL5"),
  age = c(4, 6, 12, 18),
  phenotype = c("Plaque #", "Plaque Size"),
  stringsAsFactors = FALSE
)

phenotypes_df <- dplyr::bind_rows(phenotypes_df) %>%
  dplyr::bind_rows(phenotypes_df) %>%
  dplyr::bind_rows(phenotypes_df) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    value = runif(1, 0, 50)
  ) %>%
  dplyr::ungroup()

phenotypes_df <- phenotypes_df %>%
  dplyr::mutate(
    age = as.factor(age),
    sex = as.factor(sex),
    mouse_model = ifelse(dplyr::row_number() %in% sample(1:nrow(phenotypes_df), round(nrow(phenotypes_df) / 2)), paste0("5XfAD;", mouse_model_group), mouse_model_group),
    value = ifelse(dplyr::row_number() %in% sample(1:nrow(phenotypes_df), 10), NA_real_, value)
  ) %>%
  dplyr::mutate(mouse_model = forcats::fct_relevel(mouse_model, c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")))

test_that("magora_boxplot produces box plots comparing the phenotype by mouse model, age, and sex", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "GFAP+ Cell Density",
      .data$mouse_model %in% c("C57BL6J", "5XFAD"),
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("faceted-box-plots", p)
})

test_that("magora_boxplot produces two rows of facets when 3-4 mouse models are selected", {
  p <- phenotypes_df %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_model %in% c("BL5", "BL6", "5XfAD;BL5"),
      .data$tissue %in% "Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("two-row-facets-three", p)

  p <- phenotypes_df %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_model %in% c("BL5", "BL6", "5XfAD;BL5", "5XfAD;BL6"),
      .data$tissue %in% "Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("two-row-facets-four", p)
})

test_that("magora_boxplot adds text to any facet without data", {
  p <- phenotypes %>%
    dplyr::filter(mouse_model != "5XFAD") %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia Density",
      .data$mouse_model %in% c("5XFAD", "C57BL6J"),
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-experiment", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_model != "C57BL6J") %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia Density",
      .data$mouse_model %in% c("5XFAD", "C57BL6J"),
      .data$tissue %in% "Cerebral Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-control", p)

  p <- phenotypes_df %>%
    dplyr::filter(mouse_model %in% c("5XfAD;BL5", "BL6")) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_model_group %in% c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6"),
      .data$tissue %in% "Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-interaction", p)

  p <- phenotypes_df %>%
    dplyr::filter(mouse_model %in% c("5XfAD;BL6", "BL6")) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_model_group %in% c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6"),
      .data$tissue %in% "Cortex"
    ) %>%
    expand_mouse_model_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("no-data-both", p)
})

test_that("All levels of age are shown in the plot even if not present in the filtered data", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Plasma Ab 40",
      .data$mouse_model %in% c("C57BL6J", "5XFAD"),
      .data$tissue %in% "Plasma"
    ) %>%
    expand_mouse_model_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("not-all-ages", p)
})

test_that("magora_boxplot shows facets in the order selected", {
  p <- phenotypes %>%
    dplyr::filter(
      phenotype %in% "Plasma Ab 40",
      mouse_model %in% c("5XFAD", "C57BL6J")
    ) %>%
    expand_mouse_model_factor_from_selection(c("5XFAD", "C57BL6J")) %>%
    magora_boxplot(use_theme_sage = FALSE)
  expect_doppelganger("pathology-different-order", p)
})
