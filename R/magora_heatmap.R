magora_heatmap <- function(data) {

  # Set up p-value legend transformation
  pvalue_legend_breaks <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2)
  log10_pvalue_legend_breaks <- -log10(pvalue_legend_breaks + 1)

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = sex_age, y = gene)) +
    ggplot2::geom_tile(colour = "black", fill = "white") +
    ggplot2::geom_point(ggplot2::aes(size = -log10(padj + 1), fill = log2foldchange), shape = 21) +
    ggplot2::facet_grid(cols = dplyr::vars(strain)) +
    ggplot2::scale_size(name = "Adjusted P-Value", limits = range(log10_pvalue_legend_breaks), breaks = log10_pvalue_legend_breaks, labels = pvalue_legend_breaks, range = c(1, 5)) +
    ggplot2::scale_fill_gradient2(low = "#85070C", high = "#164B6E", name = "Log 2 Fold change", limits = c(-1, 1), breaks = c(-1, 0, 1)) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title.position = "top", title.hjust = 0.5, ticks = FALSE),
      size = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    sagethemes::theme_sage() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
