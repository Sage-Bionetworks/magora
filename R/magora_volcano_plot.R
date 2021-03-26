magora_volcano_plot <- function(data, pvalue = pvalue, log_fc_cutoff = 1, pvalue_cutoff = 0.05, type = "ggplot2", facet = TRUE) {

  # Flag downregulated and upregulated genes

  data <- data %>%
    dplyr::mutate(diff_expressed = dplyr::case_when(
      .data$log2foldchange > log_fc_cutoff & .data$pvalue < pvalue_cutoff ~ "Upregulated",
      .data$log2foldchange < -log_fc_cutoff & .data$pvalue < pvalue_cutoff ~ "Downregulated",
      TRUE ~ "Not Significant"
    ))

  # Only label genes that are upregulated/downregulated

  data <- data %>%
    dplyr::mutate(label = dplyr::case_when(
      .data$diff_expressed == "Not Significant" ~ NA_character_,
      TRUE ~ .data$gene
    ))

  # Filter for non-NA data to minimize warnings (though some are expected from NA labels)

  data <- data %>%
    dplyr::filter(!is.na(.data$log2foldchange) & !is.na({{pvalue}}))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$log2foldchange, y = -log10({{pvalue}}), colour = .data$diff_expressed, text = .data$gene)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_vline(xintercept = c(-log_fc_cutoff, log_fc_cutoff), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(pvalue_cutoff), linetype = "dashed") +
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
      ggrepel::geom_text_repel(ggplot2::aes(label = .data$label), show.legend = FALSE, seed = 1234) +
      ggplot2::labs(x = bquote(~Log[2]~ "Fold change"), y = bquote(~-Log[10]~"P-Value"))
  } else if (type == "plotly") {
    p <- p +
      ggplot2::labs(x = "Log2 Fold Change", y = "Log 10 P-Value")

    plotly::ggplotly(p, tooltip = "text")
  }
}

