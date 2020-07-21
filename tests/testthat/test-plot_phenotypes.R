context("plot_phenotypes") # Context required by vdiffr still

library(vdiffr)

# Set up fake data for testing on

set.seed(1234)

df <- expand.grid(
  tissue = c("cortex", "hippocampus"),
  sex = c("Male", "Female"),
  mouse_line_group = c("BL6", "BL5"),
  age = c(4, 6, 12, 18),
  phenotype = c("Plaque #", "Plaque Size"),
  stringsAsFactors = FALSE
)

df <- dplyr::bind_rows(df) %>%
  dplyr::bind_rows(df) %>%
  dplyr::bind_rows(df) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    value = runif(1, 0, 50)
  ) %>%
  dplyr::ungroup()

df <- df %>%
  dplyr::mutate(
    age = as.factor(age),
    mouse_line = ifelse(dplyr::row_number() %in% sample(1:nrow(df), round(nrow(df) / 2)), paste0("5XfAD;", mouse_line_group), mouse_line_group),
    value = ifelse(dplyr::row_number() %in% sample(1:nrow(df), 10), NA_real_, value)
  ) %>%
  dplyr::mutate(mouse_line = forcats::fct_relevel(mouse_line, c("BL5", "5XfAD;BL5", "BL6", "5XfAD;BL6")))

test_that("plot_phenotypes produces box plots comparing the phenotype by mouse line (experiment/control, in facets), age, and sex", {
  p <- phenotypes %>%
    filter_pathology("Plaque #", "BL6", "cortex") %>%
    expand_mouse_line_factor("BL6") %>%
    plot_phenotypes()
  expect_doppelganger("faceted-box-plots", p)
})

test_that("plot_phenotypes produces two rows of facets when two mouse lines are included.", {
  p <- df %>%
    filter_pathology("Plaque #", c("BL5", "BL6"), "cortex") %>%
    expand_mouse_line_factor(c("BL5", "BL6"), df) %>%
    plot_phenotypes()
  expect_doppelganger("two-row-facets", p)
})

test_that("plot_phenotypes adds text to any facet without data", {
  p <- phenotypes %>%
    dplyr::filter(mouse_line != "5XfAD;BL6") %>%
    filter_pathology("Microglia #", "BL6", "cortex") %>%
    expand_mouse_line_factor("BL6") %>%
    plot_phenotypes()
  expect_doppelganger("no-data-experiment", p)

  p <- phenotypes %>%
    dplyr::filter(mouse_line != "BL6") %>%
    filter_pathology("Microglia #", "BL6", "cortex") %>%
    expand_mouse_line_factor("BL6") %>%
    plot_phenotypes()
  expect_doppelganger("no-data-control", p)

  p <- df %>%
    dplyr::filter(mouse_line %in% c("5XfAD;BL5", "BL6")) %>%
    filter_pathology("Plaque #", c("BL5", "BL6"), "cortex") %>%
    expand_mouse_line_factor(c("BL5", "BL6"), df) %>%
    plot_phenotypes()
  expect_doppelganger("no-data-interaction", p)

  p <- df %>%
    dplyr::filter(mouse_line %in% c("5XfAD;BL6", "BL6")) %>%
    filter_pathology("Plaque #", c("BL5", "BL6"), "cortex") %>%
    expand_mouse_line_factor(c("BL5", "BL6"), df) %>%
    plot_phenotypes()
  expect_doppelganger("no-data-both", p)
})

test_that("All levels of age are shown in the plot even if not present in the filtered data", {
  p <- phenotypes %>%
    filter_pathology("Plasma AB 40", "BL6", "plasma") %>%
    expand_mouse_line_factor("BL6") %>%
    plot_phenotypes()
  expect_doppelganger("not-all-ages", p)
})
