complete_gene_expression_heatmap_data <- function(data, input_gene, input_mouse_model) {
  data <- data %>%
    # Ensure genes selected show in plot, even if there is no data
    dplyr::mutate(
      gene = forcats::fct_drop(.data$gene),
      gene = forcats::fct_expand(.data$gene, input_gene),
      gene = forcats::fct_relevel(.data$gene, input_gene)
    ) %>%
    # Same with mouse_model
    dplyr::mutate(
      mouse_model = forcats::fct_drop(.data$mouse_model),
      mouse_model = forcats::fct_expand(.data$mouse_model, input_mouse_model),
      mouse_model = forcats::fct_relevel(.data$mouse_model, input_mouse_model)
    ) %>%
    # "Complete" the data set so there is a white square for every combination
    tidyr::complete(.data$mouse_model, .data$tissue, .data$gene, .data$sex, .data$age) %>%
    # Derive a combined sex and age field
    dplyr::mutate(sex_age = glue::glue("{.data$sex} ({.data$age} Months)")) %>%
    dplyr::arrange(.data$sex, .data$age) %>%
    dplyr::mutate(sex_age = forcats::fct_inorder(.data$sex_age))

  # Split a long mouse model onto two lines if it's present

  if ("APOE4TREM2" %in% input_mouse_model) {
    data <- data %>%
      dplyr::mutate(mouse_model = forcats::fct_recode(.data$mouse_model, `APOE4\nTREM2` = "APOE4TREM2"))
  }

  data
}
