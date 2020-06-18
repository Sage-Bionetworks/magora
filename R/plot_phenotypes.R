#' Plot phenotype data
#'
#' @param .data Input data (the result of \code{\link{filter_pathology}} with \code{\link{phenotypes}} as input).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' \donttest{
#' phenotypes %>%
#'   filter_pathology(.phenotype = "Plaque #", .mouse_line = "BL6", .tissue = "hippocampus") %>%
#'   plot_phenotypes()
#' }
plot_phenotypes <- function(.data) {

  ggplot2::ggplot(.data, ggplot2::aes(x = .data$Age, y = .data$value, fill = .data$Sex)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge2(width = 0.5, preserve = "single")) +
    ggplot2::facet_wrap(ggplot2::vars(.data$`Mouse Line`)) +
    ggplot2::labs(x = "Age", y = unique(.data[["Phenotype"]])) +
    sagethemes::scale_fill_sage_d() +
    sagethemes::theme_sage()

}
