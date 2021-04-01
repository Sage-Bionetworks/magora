magora_volcano_plot <- function(data, data_labels, type = "ggplot2", facet = TRUE, save_name, sample_frac = 1) {

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x = .data$log2foldchange, y = -log10(.data$pvalue), colour = .data$diff_expressed, text = .data$gene), alpha = 0.25) +
    ggplot2::geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
    ggplot2::scale_colour_manual(values = c("#85070C", "darkgrey", "#164B6E"), name = NULL, guide = ggplot2::guide_legend(override.aes = list(size = 3))) +
    sagethemes::theme_sage() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(legend.position = "top")

  if (facet) {
    p <- p +
      ggplot2::facet_wrap(dplyr::vars(.data$sex, .data$age), nrow = 2, scales = "free", labeller = ggplot2::labeller(age = function(x) {glue::glue("{x} Months")}))
  }

  if (type == "ggplot2") {
    p +
      ggrepel::geom_text_repel(data = data_labels, ggplot2::aes(x = .data$log2foldchange, y = -log10(.data$pvalue), colour = .data$diff_expressed, label = .data$label), show.legend = FALSE, seed = 1234, max.overlaps = 5, point.size = NA) +
      ggplot2::labs(x = bquote(~ Log[2] ~ "Fold change"), y = bquote(~ -Log[10] ~ "P-Value"))
  } else if (type == "plotly") {
    p <- p +
      ggplot2::labs(x = "Log2 Fold Change", y = "Log 10 P-Value")

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::config(
        toImageButtonOptions = list(format = "png", filename = save_name, height = 600, width = 900, scale = 2),
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "zoom3d", "zoomInGeo", "zoomOutGeo", "zoomInMapbox", "zoomOutMapbox", "autoScale2d", "resetScale2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
      )
  }
}

sample_gene_expressions <- function(data, sample_frac) {
  set.seed(1234)

  not_significant <- data %>%
    dplyr::filter(.data$diff_expressed == "Not Significant") %>%
    dplyr::group_by(.data$sex, .data$age) %>%
    dplyr::sample_frac(size = sample_frac) %>%
    dplyr::ungroup()

  data %>%
    dplyr::filter(.data$diff_expressed != "Not Significant") %>%
    dplyr::bind_rows(not_significant)
}
