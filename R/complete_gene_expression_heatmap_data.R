complete_gene_expression_heatmap_data <- function(data, input) {

  tissues <- magora::gene_expressions_tissue %>%
    unlist() %>%
    unique() %>%
    sort()

  data <- data %>%
    # Ensure genes selected show in plot, even if there is no data
    dplyr::mutate(
      gene = forcats::fct_drop(.data$gene),
      gene = forcats::fct_expand(.data$gene, input$gene),
      gene = forcats::fct_relevel(.data$gene, input$gene)
    ) %>%
    # Same with mouse_model
    dplyr::mutate(
      mouse_model = forcats::fct_drop(.data$mouse_model),
      mouse_model = forcats::fct_expand(.data$mouse_model, input$mouse_model),
      mouse_model = forcats::fct_relevel(.data$mouse_model, input$mouse_model)
    ) %>%
    # And tissue
    dplyr::mutate(
      tissue = forcats::fct_drop(.data$tissue),
      tissue = forcats::fct_expand(.data$tissue, tissues),
      tissue = forcats::fct_relevel(.data$tissue, tissues)
    ) %>%
    # And sex
    dplyr::mutate(
      sex = forcats::fct_drop(.data$sex),
      sex = forcats::fct_expand(.data$sex, input$sex),
      sex = forcats::fct_relevel(.data$sex, input$sex)
    ) %>%
    # And age
    dplyr::mutate(
      age_num = .data$age,
      age = as.character(.data$age),
      age = forcats::fct_drop(.data$age),
      age = forcats::fct_expand(.data$age, input$age),
      age = forcats::fct_relevel(.data$age, input$age)
    ) %>%
    # "Complete" the data set so there is a white square for every combination
    tidyr::complete(.data$mouse_model, .data$tissue, .data$gene, .data$sex, .data$age) %>%
    # Derive a combined sex and age field
    dplyr::mutate(sex_age = glue::glue("{.data$sex} ({.data$age} Months)")) %>%
    dplyr::arrange(.data$sex, .data$age_num) %>%
    dplyr::mutate(sex_age = forcats::fct_inorder(.data$sex_age))

  # Split a long mouse model onto two lines if it's present

  if ("APOE4TREM2" %in% input$mouse_model) {
    data <- data %>%
      dplyr::mutate(mouse_model = forcats::fct_recode(.data$mouse_model, `APOE4\nTREM2` = "APOE4TREM2"))
  }

  data
}
