# Set up fake data for testing on
test_that("expand_mouse_line_factor_from_group expands the data[['mouse_line']] factor levels to include both experiment and control lines for the specified `mouse_line_group`, even if there isn't data for both", {
  output <- phenotypes %>%
    expand_mouse_line_factor_from_group("BL6") %>%
    dplyr::pull(mouse_line) %>%
    levels()
  expect_equal(output, c("BL6", "5XfAD;BL6"))

  output <- phenotypes %>%
    dplyr::filter(mouse_line == "BL6") %>%
    expand_mouse_line_factor_from_group("BL6") %>%
    dplyr::pull(mouse_line) %>%
    levels()
  expect_equal(output, c("BL6", "5XfAD;BL6"))
})
