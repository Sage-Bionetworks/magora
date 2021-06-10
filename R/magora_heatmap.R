#' Heatmap for gene expression data
#'
#' Creates a heatmap showing the log fold change and p-value of genes across sex, age, and models.
#'
#' @param data Input data (\code{\link{gene_expressions}}, filtered by a tissue and one or more genes.
#' @param log_foldchange_cutoff Cutoff for log fold change, used in plot legend
#'
#' @return A ggplot2 object.
#' @export
magora_heatmap <- function(data, log_foldchange_cutoff = 2.5) {

  # Set up p-value legend transformation
  pvalue_breaks <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2)
  log10_pvalue_breaks <- -log10(pvalue_breaks)

  # Manually split p-values into categories to handle smaller than smallest and larger than largest
  data <- data %>%
    dplyr::mutate(
      padj_category = purrr::map_dbl(.data$padj, categorize_pvalue, pvalue_breaks),
      padj_category_log10 = -log10(.data$padj_category)
    )

  # Re-categorize anything absolutely larger than log_foldchange_cutoff so that legend can be set, but it will still have a colour
  data <- data %>%
    dplyr::mutate(log2foldchange = dplyr::case_when(
      log2foldchange < 0 & abs(log2foldchange) > log_foldchange_cutoff ~ -log_foldchange_cutoff,
      log2foldchange > 0 & abs(log2foldchange) > log_foldchange_cutoff ~ log_foldchange_cutoff,
      TRUE ~ log2foldchange
    ))

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$sex_age, y = .data$gene)) +
    ggplot2::geom_tile(colour = "black", fill = "white") +
    ggplot2::geom_point(ggplot2::aes(size = .data$padj_category_log10, fill = .data$log2foldchange), shape = 21) +
    ggplot2::facet_grid(cols = dplyr::vars(.data$mouse_model), rows = dplyr::vars(.data$tissue)) +
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

categorize_pvalue <- function(pvalue, pvalue_breaks) {
  # Separate into category numbers
  pvalue_category <- cut(pvalue, pvalue_breaks, labels = FALSE)

  # If outside of the bounds of the breaks, categorize as either 1 or the largest # category
  pvalue_category <- dplyr::case_when(
    !is.na(pvalue_category) ~ pvalue_category,
    pvalue < min(pvalue_breaks) ~ 1L,
    pvalue > max(pvalue_breaks) ~ length(pvalue_breaks)
  )

  # Get the value of the category number
  ifelse(is.na(pvalue_category), NA_real_, pvalue_breaks[[pvalue_category]])
}
