#' Expand mouse line factor from selection
#'
#' Expand \code{data[["mouse_model"]]} factor levels to include all mouse lines selected in \code{mouse_model}, even if there is not data for all. Used before \code{\link{magora_boxplot}} to ensure that facets are shown for all relevant mouse lines.
#'
#' @param data Input data (e.g. \code{\link{phenotypes}} filtered by phenotype, mouse line, and tissue).
#' @param mouse_model Mouse lines to use as factor levels.
#'
#' @export
expand_mouse_line_factor_from_selection <- function(data, mouse_model) {
  data %>%
    dplyr::mutate(
      mouse_model = forcats::fct_drop(.data$mouse_model), # Drop unused factors
      mouse_model = forcats::fct_expand(.data$mouse_model, !!mouse_model), # Expand to include selected factors that don't appear in the data
      mouse_model = forcats::fct_relevel(.data$mouse_model, !!mouse_model) # Use same order as selection
    )
}
