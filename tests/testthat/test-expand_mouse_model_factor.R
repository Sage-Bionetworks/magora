phenotype_mouse_models <- levels(phenotypes[["mouse_model"]])

test_that("expand_mouse_model_factor_from_selection only includes factors selected", {
  mouse_model_selection <- phenotype_mouse_models

  output <- phenotypes %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- phenotype_mouse_models[[1]]

  output <- phenotypes %>%
    dplyr::filter(mouse_model %in% mouse_model_selection) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- phenotype_mouse_models[[2]]

  output <- phenotypes %>%
    dplyr::filter(mouse_model %in% mouse_model_selection) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

test_that("expand_mouse_model_factor_from_selection includes factors selected, even if they don't have any data", {
  mouse_model_selection <- phenotype_mouse_models

  output <- phenotypes %>%
    dplyr::filter(mouse_model %in% phenotype_mouse_models[[2]]) %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})

test_that("expand_mouse_model_factor_from_selection returns factor levels in the same order as the selection", {
  mouse_model_selection <- phenotype_mouse_models

  output <- phenotypes %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)

  mouse_model_selection <- phenotype_mouse_models

  output <- phenotypes %>%
    expand_mouse_model_factor_from_selection(mouse_model_selection)
  expect_equal(levels(output[["mouse_model"]]), mouse_model_selection)
})
