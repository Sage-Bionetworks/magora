# Set up fake data for testing on
test_that("refactor_mouse_line expands the data[['mouse_line_full']] factor levels to include both experiment and control lines for the specified `mouse_line_full`, even if there isn't data for both", {

  df <- dplyr::tibble(mouse_line = rep("BL6", 10)) %>%
    dplyr::mutate(
      mouse_line_full = ifelse(dplyr::row_number() %in% sample(1:nrow(.), round(nrow(.) / 2)), paste0("5XfAD;", mouse_line), mouse_line),
      mouse_line_full = forcats::fct_relevel(mouse_line_full, c("BL6", "5XfAD;BL6"))
    )

  output <- df %>%
    refactor_mouse_line("BL6") %>%
    dplyr::pull(mouse_line_full) %>%
    levels()
  expect_equal(output, c("BL6", "5XfAD;BL6"))

  output <- df %>%
    dplyr::filter(mouse_line_full == "BL6") %>%
    refactor_mouse_line("BL6") %>%
    dplyr::pull(mouse_line_full) %>%
    levels()
  expect_equal(output, c("BL6", "5XfAD;BL6"))

})
