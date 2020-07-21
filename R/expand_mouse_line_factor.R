#' Expand mouse line factor
#'
#' Expand mouse line factor to include all experiment and control lines for the specified \code{mouse_line_group}, even if there is not data for both. Used before \code{\link{plot_phenotypes}} to ensure that facets are shown for all mouse lines selected.
#'
#' @param data Input data (\code{\link{phenotypes}} filtered by phenotype, mouse line, and tissue).
#' @param mouse_line_group Mouse line group(s) to expand factor for.
#' @param phenotype_data Phenotype data to pull original mouse line factors from. Defaults to \code{link{phenotypes}}.
expand_mouse_line_factor <- function(data, mouse_line_group, phenotype_data = magora::phenotypes) {
  mouse_line_experiment_control <- phenotype_data %>%
    dplyr::filter(.data$mouse_line_group %in% !!mouse_line_group) %>%
    dplyr::arrange(.data$mouse_line) %>%
    dplyr::pull(.data$mouse_line) %>%
    as.character() %>%
    unique()

  data %>%
    dplyr::mutate(
      mouse_line = forcats::fct_drop(.data$mouse_line),
      mouse_line = forcats::fct_expand(.data$mouse_line, mouse_line_experiment_control),
      mouse_line = forcats::fct_relevel(.data$mouse_line, mouse_line_experiment_control)
    )
}
