pathology_mouse_models <- levels(pathology[["mouse_model"]])

test_that("expand_mouse_model_factor_from_selection only includes factors selected", {
  mouse_model_selection <- pathology_mouse_models

  output <- pathology %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- pathology_mouse_models[[1]]

  output <- pathology %>%
    dplyr::filter(mouse_model %in% mouse_model_selection) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- pathology_mouse_models[[2]]

  output <- pathology %>%
    dplyr::filter(mouse_model %in% mouse_model_selection) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

test_that("expand_mouse_model_factor_from_selection includes factors selected, even if they don't have any data", {
  mouse_model_selection <- pathology_mouse_models

  output <- pathology %>%
    dplyr::filter(mouse_model %in% pathology_mouse_models[[2]]) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

test_that("expand_mouse_model_factor_from_selection returns factor levels in the same order as the selection", {
  mouse_model_selection <- pathology_mouse_models

  output <- pathology %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- pathology_mouse_models

  output <- pathology %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

