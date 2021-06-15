#' Regenerate p-value from log10(pvalue)
#'
#' Regenerated p-values from their log10 values, which are much easier to store.
#'
#' @param data Gene expression data (\link{gene_expressions})
#'
#' @examples
regenerate_pvalue <- function(data) {
  data %>%
    dplyr::mutate(
      padj = 10^(-.data$neg_log10_padj)
    )
}
