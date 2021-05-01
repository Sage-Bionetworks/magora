complete_gene_expression_heatmap_data <- function(data) {
  data %>%
    tidyr::complete(strain, tissue, gene, sex, age) %>%
    dplyr::mutate(sex_age = paste(sex, age)) %>%
    dplyr::arrange(sex, age) %>%
    dplyr::mutate(sex_age = forcats::fct_inorder(sex_age))
}
