#' Expand mouse line factor
#'
#' Expand mouse line factor to include all experiment and control lines for the specified \code{mouse_line}, even if there is not data for both. Used before \code{\link{plot_phenotypes}} to ensure that facets are shown for all mouse lines selected.
#'
#' @param data Input data (the result of \code{\link{filter_pathology}} with \code{\link{phenotypes}} as input).
#' @param mouse_line Mouse line(s) to expand factor for.
#' @param phenotype_data Phenotype data to pull original mouse line factors from. Defaults to \code{link{phenotypes}}.
expand_mouse_line_factor <- function(data, mouse_line, phenotype_data = phenotypes) {
  mouse_line_experiment_control <- phenotype_data %>%
    dplyr::filter(.data$mouse_line %in% !!mouse_line) %>%
    dplyr::arrange(.data$mouse_line_full) %>%
    dplyr::pull(.data$mouse_line_full) %>%
    as.character() %>%
    unique()

  data %>%
    dplyr::mutate(
      mouse_line_full = forcats::fct_drop(.data$mouse_line_full),
      mouse_line_full = forcats::fct_expand(.data$mouse_line_full, mouse_line_experiment_control),
      mouse_line_full = forcats::fct_relevel(.data$mouse_line_full, mouse_line_experiment_control)
    )
}
