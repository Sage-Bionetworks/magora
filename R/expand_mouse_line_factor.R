#' Expand mouse line factor from selection
#'
#' Expand \code{data[["mouse_line"]]} factor levels to include all mouse lines selected in \code{mouse_line}, even if there is not data for all. Used before \code{\link{magora_boxplot}} to ensure that facets are shown for all relevant mouse lines.
#'
#' @param data Input data (e.g. \code{\link{gene_expressions}} filtered by phenotype, mouse line, and tissue).
#' @param mouse_line Mouse lines to use as factor levels.
#'
#' @export
expand_mouse_line_factor_from_selection <- function(data, mouse_line) {
  data %>%
    dplyr::mutate(
      mouse_line = forcats::fct_drop(.data$mouse_line), # Drop unused factors
      mouse_line = forcats::fct_expand(.data$mouse_line, !!mouse_line), # Expand to include selected factors that don't appear in the data
      mouse_line = forcats::fct_relevel(.data$mouse_line, !!mouse_line) # Use same order as selection
    )
}

#' Expand mouse line factor from group
#'
#' Expand \code{data[["mouse_line"]]} factor levels to include all experiment and control lines for the specified \code{mouse_line_group}, even if there is not data for both. Used before \code{\link{magora_boxplot}} to ensure that facets are shown for all relevant mouse lines.
#'
#' @param data Input data (e.g. \code{\link{phenotypes}} filtered by phenotype, mouse line group, and tissue).
#' @param mouse_line_group Mouse line group(s) to expand factor for.
#' @param reference_data Reference data to pull original mouse line factor levels from. Defaults to \code{\link{phenotypes}}.
#'
#' @export
expand_mouse_line_factor_from_group <- function(data, mouse_line_group, reference_data = magora::phenotypes) {
  mouse_line_experiment_control <- reference_data %>%
    dplyr::filter(.data$mouse_line_group %in% !!mouse_line_group) %>%
    dplyr::arrange(.data$mouse_line) %>%
    dplyr::pull(.data$mouse_line) %>%
    as.character() %>%
    unique()

  expand_mouse_line_factor_from_selection(data, mouse_line_experiment_control)
}
