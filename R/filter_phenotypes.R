filter_phenotypes <- function(.data, .phenotype, .mouse_line, .tissue, na.rm = TRUE) {
  .data <- .data %>%
    dplyr::filter(
      .data$Phenotype == .phenotype,
      .data$mouse_line %in% .mouse_line,
      .data$Tissue == .tissue)

  if (na.rm) {
    .data <- .data %>%
      dplyr::filter(!is.na(.data$value))
  }

  .data
}
