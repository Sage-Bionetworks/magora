library(vdiffr)
context("magora_boxplot") # Required by vdiffr still

# Phenotypes ----

# Set up fake data for testing on

set.seed(1234)

phenotypes_df <- expand.grid(
  tissue = c("cortex", "hippocampus"),
  sex = c("Male", "Female"),
  mouse_line_group = c("BL6", "BL5"),
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
    mouse_line = ifelse(dplyr::row_number() %in% sample(1:nrow(phenotypes_df), round(nrow(phenotypes_df) / 2)), paste0("5XfAD;", mouse_line_group), mouse_line_group),
    value = ifelse(dplyr::row_number() %in% sample(1:nrow(phenotypes_df), 10), NA_real_, value)
  ) %>%
  dplyr::mutate(mouse_line = forcats::fct_relevel(mouse_line, c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")))

test_that("magora_boxplot produces box plots comparing the phenotype by mouse line, age, and sex", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "GFAP+ cell #",
      .data$mouse_line %in% c("C57BL6J", "5XFAD"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot()
  expect_doppelganger("faceted-box-plots", p)
})

test_that("magora_boxplot produces two rows of facets when 3-4 mouse lines are selected", {
  p <- phenotypes_df %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line %in% c("BL5", "BL6", "5XfAD;BL5"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6")) %>%
    magora_boxplot()
  expect_doppelganger("two-row-facets-three", p)

  p <- phenotypes_df %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line %in% c("BL5", "BL6", "5XfAD;BL5", "5XfAD;BL6"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")) %>%
    magora_boxplot()
  expect_doppelganger("two-row-facets-four", p)
})

test_that("magora_boxplot adds text to any facet without data", {
  p <- phenotypes %>%
    dplyr::filter(mouse_line != "5XFAD") %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia #",
      .data$mouse_line %in% c("5XFAD", "C57BL6J"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot()
  expect_doppelganger("no-data-experiment", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_line != "C57BL6J") %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia #",
      .data$mouse_line %in% c("5XFAD", "C57BL6J"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot()
  expect_doppelganger("no-data-control", p)

  p <- phenotypes_df %>%
    dplyr::filter(mouse_line %in% c("5XfAD;BL5", "BL6")) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line_group %in% c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")) %>%
    magora_boxplot()
  expect_doppelganger("no-data-interaction", p)

  p <- phenotypes_df %>%
    dplyr::filter(mouse_line %in% c("5XfAD;BL6", "BL6")) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line_group %in% c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor_from_selection(c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")) %>%
    magora_boxplot()
  expect_doppelganger("no-data-both", p)
})

test_that("All levels of age are shown in the plot even if not present in the filtered data", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Plasma AB 40",
      .data$mouse_line %in% c("C57BL6J", "5XFAD"),
      .data$tissue %in% "plasma"
    ) %>%
    expand_mouse_line_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot()
  expect_doppelganger("not-all-ages", p)

  p <- gene_expressions %>%
    dplyr::filter(
      gene == "Zfp423",
      .data$mouse_line %in% c("C57BL6J", "5XFAD"),
    ) %>%
    dplyr::filter(!(mouse_line == "C57BL6J" & age == 4)) %>%
    expand_mouse_line_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot()
  expect_doppelganger("not-all-ages-gene-expressions", p)
})

test_that("magora_boxplot with plot_type = 'gene expression' uses 'gene expression' in annotations and has a TPM y-axis label", {
  p <- gene_expressions %>%
    dplyr::filter(
      gene == "Zfp423",
      mouse_line == "5XFAD"
    ) %>%
    expand_mouse_line_factor_from_selection(c("5XFAD", "C57BL6J")) %>%
    magora_boxplot(plot_type = "gene expression")
  expect_doppelganger("gene-expression-no-data", p)
})

test_that("magora_boxplot shows all, and only, mouse lines selected", {
  p <- gene_expressions %>%
    dplyr::filter(
      gene == "Zfp423",
      mouse_line == "5XFAD"
    ) %>%
    expand_mouse_line_factor_from_selection("5XFAD") %>%
    magora_boxplot(plot_type = "gene expression")
  expect_doppelganger("gene-expression-single-mouse-line", p)

  p <- gene_expressions %>%
    dplyr::filter(
      gene == "Zfp423",
      mouse_line == c("5XFAD", "C57BL6J")
    ) %>%
    expand_mouse_line_factor_from_selection(c("5XFAD", "C57BL6J")) %>%
    magora_boxplot(plot_type = "gene expression")
  expect_doppelganger("gene-expression-all-mouse-line", p)
})

test_that("magora_boxplot shows facets in the order selected", {
  p <- gene_expressions %>%
    dplyr::filter(
      gene == "Zfp423",
      mouse_line == c("5XFAD", "C57BL6J")
    ) %>%
    expand_mouse_line_factor_from_selection(c("C57BL6J", "5XFAD")) %>%
    magora_boxplot(plot_type = "gene expression")
  expect_doppelganger("gene-expression-different-order", p)

  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Plasma AB 40",
      .data$mouse_line == c("5XFAD", "C57BL6J")
    ) %>%
    expand_mouse_line_factor_from_selection(c("5XFAD", "C57BL6J")) %>%
    magora_boxplot()
  expect_doppelganger("pathology-different-order", p)
})