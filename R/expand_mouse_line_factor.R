#' Expand mouse line factor from selection
#'
#' Expand \code{data[["mouse_line"]]} factor levels to include all mouse lines selected in \code{mouse_line}, even if there is not data for all. Used before \code{\link{magora_boxplot}} to ensure that facets are shown for all relevant mouse lines.
#'
#' @param data Input data (e.g. \code{\link{phenotypes}} filtered by phenotype, mouse line, and tissue).
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
