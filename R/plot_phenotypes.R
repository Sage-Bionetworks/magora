#' Plot phenotype data
#'
#' @param .data Input data (the result of \code{\link{filter_pathology}} with \code{\link{phenotypes}} as input).
#'
#' @return A ggplot2 object.
#' @export
plot_phenotypes <- function(.data) {

  ggplot2::ggplot(.data, ggplot2::aes(x = .data$age, y = .data$value, fill = .data$sex)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge2(width = 0.5, preserve = "single")) +
    ggplot2::facet_wrap(ggplot2::vars(.data$mouse_line_full), ncol = 2, drop = FALSE) +
    ggplot2::labs(x = "Age", y = unique(.data[["phenotype"]])) +
    sagethemes::scale_fill_sage_d() +
    sagethemes::theme_sage()

}
