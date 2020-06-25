#' Filter pathology data
#'
#' Filter pathology data to only include data from the specified \code{phenotype}, \code{mouse_line}, and \code{tissue} values.
#'
#' @param data Input data (e.g. \code{\link{phenotypes}})
#' @param phenotype Phenotype to filter by. Keeps data where \code{.data[["phenotype"]] \%in\% phenotype}
#' @param mouse_line Mouse lines to filter by. Keeps data where \code{.data[["mouse_line"]] \%in\% mouse_line}
#' @param tissue Tissue to filter by. Keeps data where \code{.data[["tissue"]] \%in\% tissue}
#'
#' @export
#'
#' @examples
#' phenotypes %>%
#'   filter_pathology(phenotype = "Plaque #", mouse_line = "BL6", tissue = "hippocampus")
filter_pathology <- function(data, phenotype, mouse_line, tissue) {
  data %>%
    dplyr::filter(
      .data$phenotype %in% !!phenotype,
      .data$mouse_line %in% !!mouse_line,
      .data$tissue %in% !!tissue
    )
}
