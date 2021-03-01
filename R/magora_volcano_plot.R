magora_volcano_plot <- function(data, pvalue = pvalue, log_fc_cutoff = 1, pvalue_cutoff = 0.05) {

  # Flag downregulated and upregulated genes

  data <- data %>%
    dplyr::mutate(diff_expressed = dplyr::case_when(
      log2fold_change > log_fc_cutoff & pvalue < pvalue_cutoff ~ "Upregulated",
      log2fold_change < -log_fc_cutoff & pvalue < pvalue_cutoff ~ "Downregulated",
      TRUE ~ "Not Significant"
    ))

  # Only label genes that are upregulated/downregulated

  data <- data %>%
    dplyr::mutate(label = dplyr::case_when(
      diff_expressed == "Not Significant" ~ NA_character_,
      TRUE ~ gene
    ))

  # Filter for non-NA data to minimize warnings (though some are expected from NA labels)

  data <- data %>%
    dplyr::filter(!is.na(log2fold_change) & !is.na({{pvalue}}))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = log2fold_change, y = -log10({{pvalue}}), colour = diff_expressed)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_vline(xintercept = c(-log_fc_cutoff, log_fc_cutoff), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(pvalue_cutoff), linetype = "dashed") +
    # ggrepel::geom_text_repel(ggplot2::aes(label = label), show.legend = FALSE, seed = 1234) +
    ggplot2::scale_colour_manual(values = c("#164B6E", "darkgrey", "#85070C"), name = NULL)

  # +
  #   ggplot2::labs(x = bquote(~Log[2]~ "Fold change"), y = bquote(~-Log[10]~"P-Value")) +
  #   sagethemes::theme_sage()

  plotly::ggplotly(p)
}
