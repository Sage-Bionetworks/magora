complete_gene_expression_heatmap_data <- function(data, input_gene) {
  data %>%
    # Ensure genes selected show in plot, even if there is no data
    dplyr::mutate(
      gene = forcats::fct_drop(.data$gene),
      gene = forcats::fct_expand(.data$gene, input_gene),
      gene = forcats::fct_relevel(.data$gene, input_gene),
      gene = forcats::fct_rev(.data$gene)
    ) %>%
    # "Complete" the data set so there is a white square for every combination
    tidyr::complete(.data$strain, .data$tissue, .data$gene, .data$sex, .data$age) %>%
    # Derive a combined sex and age field
    dplyr::mutate(sex_age = paste(.data$sex, .data$age)) %>%
    dplyr::arrange(.data$sex, .data$age) %>%
    dplyr::mutate(sex_age = forcats::fct_inorder(.data$sex_age))
}
