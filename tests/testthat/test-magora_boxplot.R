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

test_that("magora_boxplot produces box plots comparing the phenotype by mouse line (experiment/control, in facets), age, and sex", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "GFAP+ cell #",
      .data$mouse_line_group %in% "BL6",
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor("BL6") %>%
    magora_boxplot()
  expect_doppelganger("faceted-box-plots", p)
})

test_that("magora_boxplot produces two rows of facets when two mouse lines are included.", {
  p <- phenotypes_df %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line_group %in% c("BL5", "BL6"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor(c("BL5", "BL6"), phenotypes_df) %>%
    magora_boxplot()
  expect_doppelganger("two-row-facets", p)
})

test_that("magora_boxplot adds text to any facet without data", {
  p <- phenotypes %>%
    dplyr::filter(mouse_line != "5XfAD;BL6") %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia #",
      .data$mouse_line_group %in% "BL6",
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor("BL6") %>%
    magora_boxplot()
  expect_doppelganger("no-data-experiment", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_line != "BL6") %>%
    dplyr::filter(
      .data$phenotype %in% "Microglia #",
      .data$mouse_line_group %in% "BL6",
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor("BL6") %>%
    magora_boxplot()
  expect_doppelganger("no-data-control", p)

  p <- phenotypes_df %>%
    dplyr::filter(mouse_line %in% c("5XfAD;BL5", "BL6")) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line_group %in% c("BL5", "BL6"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor(c("BL5", "BL6"), phenotypes_df) %>%
    magora_boxplot()
  expect_doppelganger("no-data-interaction", p)

  p <- phenotypes_df %>%
    dplyr::filter(mouse_line %in% c("5XfAD;BL6", "BL6")) %>%
    dplyr::filter(
      .data$phenotype %in% "Plaque #",
      .data$mouse_line_group %in% c("BL5", "BL6"),
      .data$tissue %in% "cortex"
    ) %>%
    expand_mouse_line_factor(c("BL5", "BL6"), phenotypes_df) %>%
    magora_boxplot()
  expect_doppelganger("no-data-both", p)
})

test_that("All levels of age are shown in the plot even if not present in the filtered data", {
  p <- phenotypes %>%
    dplyr::filter(
      .data$phenotype %in% "Plasma AB 40",
      .data$mouse_line_group %in% "BL6",
      .data$tissue %in% "plasma"
    ) %>%
    expand_mouse_line_factor("BL6") %>%
    magora_boxplot()
  expect_doppelganger("not-all-ages", p)
})

# Gene expressions ----

gene_expression_df <- gene_expressions %>%
  dplyr::filter(
    gene_id == "ENSMUSG00000061356",
    mouse_line == "5XFAD"
  )

test_that("magora_boxplot with plot_type = 'gene expression' uses 'gene expression' in annotations and has a TPM y-axis label", {
  p <- gene_expression_df %>%
    dplyr::mutate(mouse_line = forcats::fct_expand(mouse_line, c("5XFAD", "C57BL6J"))) %>%
    magora_boxplot(plot_type = "gene expression")
  expect_doppelganger("no-data-gene-expression", p)
})
