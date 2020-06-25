#' Plot phenotype data
#'
#' @param data Input data (the result of \code{\link{filter_pathology}} and \code{\link{expand_mouse_line_factor}} with \code{\link{phenotypes}} as input).
#'
#' @return A ggplot2 object.
#' @export
plot_phenotypes <- function(data) {

  # Generate annotation for mouse lines (facets) that won't have any data
  measured_annotation <- data %>%
    dplyr::count(.data$mouse_line_full, .drop = FALSE) %>%
    dplyr::filter(.data$n == 0) %>%
    dplyr::mutate(label = "This phenotype cannot be\nmeasured in this mouse line.")

  p <- ggplot2::ggplot(data) +
    ggplot2::facet_wrap(ggplot2::vars(.data$mouse_line_full), ncol = 2, drop = FALSE) +
    ggplot2::geom_boxplot(ggplot2::aes(x = .data$age, y = .data$value, fill = paste("Sex:", .data$sex)), position = ggplot2::position_dodge2(preserve = "single"))

  if (nrow(measured_annotation) > 0) {
    y_range <- ggplot2::layer_scales(p)$y$range$range
    y_mid <- (y_range[[2]] - y_range[[1]]) / 2

    p <- p +
      ggplot2::geom_text(data = measured_annotation, mapping = ggplot2::aes(x = 2.5, y = y_mid, label = .data$label), size = 4, vjust = 0.5)
  }

  p +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = "Age", y = unique(data[["phenotype"]])) +
    sagethemes::scale_fill_sage_d() +
    sagethemes::theme_sage(base_size = 16) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}
