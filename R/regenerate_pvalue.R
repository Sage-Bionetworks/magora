#' Regenerate p-values from log10(pvalue)
#'
#' Regenerated p-values from their log10 values, which are much easier to store.
#'
#' @param data Gene expression data (\link{gene_expressions})
#'
#' @examples
regenerate_pvalue <- function(data) {
  data %>%
    dplyr::mutate(
      pvalue = 10^(-.data$neg_log10_pvalue),
      padj = 10^(-.data$neg_log10_padj)
    ) %>%
    dplyr::select(-.data$neg_log10_pvalue)
}
