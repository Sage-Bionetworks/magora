#' Plot phenotype data
#'
#' @param .data Input data (the result of \code{\link{filter_pathology}} with \code{\link{phenotypes}} as input).
#'
#' @return A ggplot2 object.
#' @export
plot_phenotypes <- function(.data) {
  measured_annotation <- .data %>%
    dplyr::group_by(mouse_line_full) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    tidyr::complete(mouse_line_full) %>%
    dplyr::filter(is.na(n)) %>%
    dplyr::mutate(text = "This phenotype cannot be\nmeasured in this mouse line.")

  p <- ggplot2::ggplot() +
    ggplot2::facet_wrap(ggplot2::vars(.data$mouse_line_full), ncol = 2, drop = FALSE) +
    ggplot2::geom_boxplot(data = .data, ggplot2::aes(x = .data$age, y = .data$value, fill = paste("Sex:", .data$sex)), position = ggplot2::position_dodge2(width = 0.5, preserve = "single"))

  if (nrow(measured_annotation) > 0) {
    y_range <- ggplot2::layer_scales(p)$y$range$range
    y_mid <- (y_range[[2]] - y_range[[1]]) / 2

    p <- p +
      ggplot2::geom_text(data = measured_annotation, mapping = ggplot2::aes(x = 2.5, y = y_mid, label = text), size = 3, vjust = 0.5)
  }

  p +
    ggplot2::labs(x = "Age", y = unique(.data[["phenotype"]])) +
    sagethemes::scale_fill_sage_d() +
    sagethemes::theme_sage() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}
