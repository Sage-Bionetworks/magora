complete_gene_expression_heatmap_data <- function(data, input_gene) {
  data %>%
    # Ensure genes selected show in plot, even if there is no data
    dplyr::mutate(
      gene = forcats::fct_drop(gene),
      gene = forcats::fct_expand(gene, input_gene),
      gene = forcats::fct_relevel(gene, input_gene),
      gene = forcats::fct_rev(gene)
    ) %>%
    # "Complete" the data set so there is a white square for every combination
    tidyr::complete(strain, tissue, gene, sex, age) %>%
    # Derive a combined sex and age field
    dplyr::mutate(sex_age = paste(sex, age)) %>%
    dplyr::arrange(sex, age) %>%
    dplyr::mutate(sex_age = forcats::fct_inorder(sex_age))
}
