#' Correlation plot for nanostring versus AMPAD data
#'
#' Creates a correlation plot comparing nanostring data to human genetic data, faceted by age group and cluster.
#'
#' @param data Input data (\code{\link{nanostring_for_plot}}
#'
#' @return A ggplot2 object.
magora_corrplot <- function(data) {

  # Set up p-value legend transformation
  pvalue_breaks <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2)
  log10_pvalue_breaks <- -log10(pvalue_breaks)

  # Manually split p-values into categories to handle smaller than smallest and larger than largest
  data <- data %>%
    dplyr::mutate(
      pvalue_category = purrr::map_dbl(.data$p_value, categorize_pvalue, pvalue_breaks),
      pvalue_category_log10 = -log10(.data$pvalue_category)
    )

  ggplot2::ggplot(data, ggplot2::aes(x = .data$module, y = .data$model_sex)) +
    ggplot2::geom_tile(colour = "black", fill = "white") +
    ggplot2::geom_point(ggplot2::aes(fill = .data$correlation, size = .data$pvalue_category_log10), shape = 21) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_size(
      name = "P-Value",
      limits = range(log10_pvalue_breaks),
      breaks = log10_pvalue_breaks,
      labels = pvalue_breaks,
      range = c(1, 6)
    ) +
    ggplot2::scale_fill_gradient2(limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5), low = "#85070C", high = "#164B6E", name = "Correlation") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_grid(rows = dplyr::vars(.data$age_group), cols = dplyr::vars(.data$cluster_label), scales = "free", space = "free") +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title.position = "top", title.hjust = 0.5, ticks = FALSE),
      size = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)
    ) +
    sagethemes::theme_sage(base_size = 12) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
      panel.background = ggplot2::element_blank(),
      plot.title.position = "plot",
      panel.grid = ggplot2::element_blank(),
      legend.position = "top"
    )
}
