test_that("expand_mouse_line_factor_from_selection only includes factors selected", {
  mouse_line_selection <- c("C57BL6J", "5XFAD")

  output <- gene_expressions %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  output <- phenotypes %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  mouse_line_selection <- "C57BL6J"

  output <- gene_expressions %>%
    dplyr::filter(mouse_line == mouse_line_selection) %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  output <- phenotypes %>%
    dplyr::filter(mouse_line == mouse_line_selection) %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  mouse_line_selection <- "5XFAD"

  output <- gene_expressions %>%
    dplyr::filter(mouse_line == mouse_line_selection) %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  output <- phenotypes %>%
    dplyr::filter(mouse_line == mouse_line_selection) %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)
})

test_that("expand_mouse_line_factor_from_selection includes factors selected, even if they don't have any data", {
  mouse_line_selection <- c("C57BL6J", "5XFAD")

  output <- gene_expressions %>%
    dplyr::filter(mouse_line == "5XFAD") %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  output <- phenotypes %>%
    dplyr::filter(mouse_line == "5XFAD") %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)
})

test_that("expand_mouse_line_factor_from_selection returns factor levels in the same order as the selection", {
  mouse_line_selection <- c("C57BL6J", "5XFAD")

  output <- gene_expressions %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  output <- phenotypes %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  mouse_line_selection <- c("5XFAD", "C57BL6J")

  output <- gene_expressions %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)

  output <- phenotypes %>%
    expand_mouse_line_factor_from_selection(mouse_line_selection)
  expect_equal(levels(output[["mouse_line"]]), mouse_line_selection)
})
