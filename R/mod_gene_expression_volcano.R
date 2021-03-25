#' Gene Expression Volcano page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_volcano_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Gene Expression (Static)"

  shiny::tabPanel(
    title,
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        "Please select a strain and tissue from the dropdown lists.",
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("strain"),
            "Strain",
            choices = sort(unique(magora::gene_expressions_for_volcano[["strain"]])),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = sort(unique(magora::gene_expressions_for_volcano[["tissue"]])),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 2,
          offset = 2,
          style = "margin-top: 27.85px",
          mod_download_plot_ui(ns("download_plot"))
        )
      ),
      shiny::column(
        width = 12,
        align = "center",
        shiny::uiOutput(ns("gene_expression_plot_ui"))
      )
      # )
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_volcano_server <- function(input, output, session, gene_expressions) {
  ns <- session$ns

  # Update tissue options available based on strain selected -----

  shiny::observeEvent(input$strain, {
    available_tissue <- sort(magora::gene_expressions_tissue[[input$strain]])

    # If the tissue previously selected is still available, keep it selected
    selected_tissue <- ifelse(input$tissue %in% available_tissue, input$tissue, available_tissue[[1]])

    shinyWidgets::updatePickerInput(
      session = session,
      "tissue",
      choices = available_tissue,
      selected = selected_tissue
    )
  })

  # Filter data based on inputs ----

  filtered_gene_expressions <- shiny::reactive({
    magora::gene_expressions_for_volcano %>%
      dplyr::filter(
        .data$strain == input$strain,
        .data$tissue == input$tissue
      )
  })

  # Generate plot ----

  gene_expression_plot <- shiny::reactive({
    filtered_gene_expressions() %>%
      dplyr::sample_n(10000) %>%
      magora_volcano_plot()
  })

  output$gene_expression_plot <- shiny::renderCachedPlot(gene_expression_plot(),
    cacheKeyExpr = {
      list(
        input$strain,
        input$tissue
      )
    },
    res = 96
  )

  gene_expression_plot_dims <- shiny::reactive({
    list(
      nrow = length(unique(filtered_gene_expressions()[["sex"]])),
      ncol = length(unique(filtered_gene_expressions()[["age"]]))
    )
  })

  output$gene_expression_plot_ui <- shiny::renderUI({
    shinycssloaders::withSpinner(shiny::plotOutput(ns("gene_expression_plot"),
      height = paste0(gene_expression_plot_dims()[["nrow"]] * 400, "px"),
      width = ifelse(gene_expression_plot_dims()[["ncol"]] == 1, "60%", "100%"),
      click = ns("plot_click")
    ),
    color = "#D3DCEF"
    )
  })

  drilldown_gene_expressions <- reactive({
    req(input$plot_click)
    panel_filter <- glue::glue('{input$plot_click$mapping$panelvar1} == "{input$plot_click$panelvar1}" & {input$plot_click$mapping$panelvar2} == "{input$plot_click$panelvar2}"')
    filtered_gene_expressions() %>%
      dplyr::filter(eval(rlang::parse_expr(panel_filter)))
  })

  drilldown_gene_expressions_title <- reactive({
    glue::glue("Strain: {input$strain}, Tissue: {input$tissue}, Sex: {input$plot_click$panelvar2}, Age: {input$plot_click$panelvar1} Months")
  })

  output$drilldown_gene_expressions <- plotly::renderPlotly({
    drilldown_gene_expressions() %>%
      dplyr::sample_n(1000) %>%
      magora_volcano_plotly()
  })

  shiny::observeEvent(input$plot_click, {
    shiny::showModal(
      shiny::modalDialog(
      size = "l",
      easyClose = TRUE,
      title = drilldown_gene_expressions_title(),
      plotly::plotlyOutput(ns("drilldown_gene_expressions"))
    ))
  })

  # Save output ----

  gene_expression_data_download <- shiny::reactive({
    filtered_gene_expressions()
  })

  save_name <- shiny::reactive({
    download_name("gene_expression", input$strain)
  })

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plot = gene_expression_plot,
    data = filtered_gene_expressions,
    save_name = save_name,
    plot_dims = gene_expression_plot_dims
  )
}
