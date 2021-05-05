magora_heatmap <- function(data, log_foldchange_cutoff = 2.5) {

  # Set up p-value legend transformation
  pvalue_breaks <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2)
  log10_pvalue_breaks <- -log10(pvalue_breaks)

  # Manually split p-values into categories to handle smaller than smallest and larger than largest
  data <- categorize_pvalues(data, padj, pvalue_breaks, log10_pvalue_breaks)

  # Re-categorize anything absolutely larger than log_foldchange_cutoff so that legend can be set, but it will still have a colour
  data <- data %>%
    dplyr::mutate(log2foldchange = dplyr::case_when(
      log2foldchange < 0 & abs(log2foldchange) > log_foldchange_cutoff ~ -log_foldchange_cutoff,
      log2foldchange > 0 & abs(log2foldchange) > log_foldchange_cutoff ~ log_foldchange_cutoff,
      TRUE ~ log2foldchange
    ))


  data %>%
    ggplot2::ggplot(ggplot2::aes(x = sex_age, y = gene)) +
    ggplot2::geom_tile(colour = "black", fill = "white") +
    ggplot2::geom_point(ggplot2::aes(size = padj_category_log10, fill = log2foldchange), shape = 21) +
    ggplot2::facet_grid(cols = dplyr::vars(strain)) +
    ggplot2::scale_size(
      name = "Adjusted P-Value",
      limits = range(log10_pvalue_breaks),
      breaks = log10_pvalue_breaks,
      labels = pvalue_breaks,
      range = c(1, 6)
    ) +
    ggplot2::scale_fill_gradient2(low = "#85070C", high = "#164B6E", name = "Log 2 Fold change", limits = c(-log_foldchange_cutoff, log_foldchange_cutoff), breaks = c(-log_foldchange_cutoff, 0, log_foldchange_cutoff)) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title.position = "top", title.hjust = 0.5, ticks = FALSE),
      size = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)
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

categorize_pvalues <- function(data, pvalue, pvalue_breaks, log10_pvalue_breaks) {
  data %>%
    dplyr::mutate(
      padj_category = cut({{ pvalue }}, pvalue_breaks, labels = FALSE),
      padj_category = dplyr::case_when(
        !is.na(.data$padj_category) ~ .data$padj_category,
        {{ pvalue }} < min(pvalue_breaks) ~ 1L,
        {{ pvalue }} > max(pvalue_breaks) ~ length(pvalue_breaks)
      ),
      padj_category = purrr::map_dbl(.data$padj_category, ~ ifelse(is.na(.x), NA_real_, pvalue_breaks[[.x]])),
      padj_category_log10 = -log10(.data$padj_category)
    )
}
