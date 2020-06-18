refactor_mouse_line <- function(.data, .mouse_line, .phenotype_data) {
  mouse_line_experiment_control <- .phenotype_data %>%
    dplyr::filter(mouse_line %in% .mouse_line) %>%
    dplyr::arrange(mouse_line_full) %>%
    dplyr::pull(mouse_line_full) %>%
    as.character() %>%
    unique()

  .data %>%
    dplyr::mutate(
      mouse_line_full = forcats::fct_drop(mouse_line_full),
      mouse_line_full = forcats::fct_expand(mouse_line_full, mouse_line_experiment_control),
      mouse_line_full = forcats::fct_relevel(mouse_line_full, mouse_line_experiment_control)
    )
}
