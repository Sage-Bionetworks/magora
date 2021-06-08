test_that("expand_mouse_model_factor_from_selection only includes factors selected", {
  mouse_model_selection <- c("C57BL6J", "5XFAD")

  output <- phenotypes %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- "C57BL6J"

  output <- phenotypes %>%
    dplyr::filter(mouse_model %in% mouse_model_selection) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- "5XFAD"

  output <- phenotypes %>%
    dplyr::filter(mouse_model %in% mouse_model_selection) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

test_that("expand_mouse_model_factor_from_selection includes factors selected, even if they don't have any data", {
  mouse_model_selection <- c("C57BL6J", "5XFAD")

  output <- phenotypes %>%
    dplyr::filter(mouse_model %in% "5XFAD") %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

test_that("expand_mouse_model_factor_from_selection returns factor levels in the same order as the selection", {
  mouse_model_selection <- c("C57BL6J", "5XFAD")

  output <- phenotypes %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- c("5XFAD", "C57BL6J")

  output <- phenotypes %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})
