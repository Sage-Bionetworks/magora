# Set up fake data for testing on
test_that("refactor_mouse_line expands the data[['mouse_line_full']] factor levels to include both experiment and control lines for the specified `mouse_line_full`, even if there isn't data for both", {
  output <- phenotypes %>%
    expand_mouse_line_factor("BL6") %>%
    dplyr::pull(mouse_line_full) %>%
    levels()
  expect_equal(output, c("BL6", "5XfAD;BL6"))

  output <- phenotypes %>%
    dplyr::filter(mouse_line_full == "BL6") %>%
    expand_mouse_line_factor("BL6") %>%
    dplyr::pull(mouse_line_full) %>%
    levels()
  expect_equal(output, c("BL6", "5XfAD;BL6"))
})
