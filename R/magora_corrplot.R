#' Correlation plot for nanostring versus AMPAD data
#'
#' Creates a correlation plot comparing nanostring data to human genetic data, faceted by age group and cluster.
#'
#' @param data Input data (\code{\link{nanostring_for_plot}}
#'
#' @return A ggplot2 object.
magora_corrplot <- function(data) {

  ggplot2::ggplot() +
    ggplot2::geom_tile(data = data, ggplot2::aes(x = .data$module, y = .data$model_sex), colour = "black", fill = "white") +
    ggplot2::geom_point(data = dplyr::filter(data, .data$significant), ggplot2::aes(x = .data$module, y = .data$model_sex, colour = .data$correlation, size = abs(.data$correlation))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_size(guide = "none", limits = c(0, 0.5)) +
    ggplot2::scale_color_gradient2(limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5), low = "#164B6E", high = "#85070C", name = "Correlation", guide = ggplot2::guide_colorbar(ticks = FALSE)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_grid(rows = dplyr::vars(.data$age_group), cols = dplyr::vars(.data$cluster_label), scales = "free", space = "free") +
    sagethemes::theme_sage(base_size = 10) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 7),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
      panel.background = ggplot2::element_blank(),
      plot.title.position = "plot",
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
