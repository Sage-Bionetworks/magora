#' Gene Expression Heatmap page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Heatmap"

  shiny::tabPanel(
    title,
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        "Please select genes and a tissue from the dropdown.",
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("gene"),
            "Genes",
            choices = sort(unique(magora::gene_expressions[["gene"]])),
            selected = sort(unique(magora::gene_expressions[["gene"]]))[[1]],
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = sort(unique(magora::gene_expressions[["tissue"]])),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 2,
          style = "margin-top: 27.85px;",
          mod_download_data_ui(ns("download_data"))
        ),
        shiny::column(
          width = 2,
          style = "margin-top: 27.85px;",
          mod_download_plot_ui(ns("download_plot"))
        ),
      ),
      shiny::column(
        width = 12,
        align = "center",
        shiny::uiOutput(ns("gene_expression_heatmap_ui"))
      )
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_heatmap_server <- function(input, output, session, gene_expressions) {
  ns <- session$ns

  # Filter data based on inputs ----

  filtered_gene_expressions <- shiny::reactive({
    magora::gene_expressions %>%
      dplyr::filter(
        .data$gene == input$gene,
        .data$tissue == input$tissue
      )
  })

  # Generate plot ----

  gene_expression_heatmap <- shiny::reactive({
    filtered_gene_expressions() %>%
      magora_heatmap()
  })

  output$gene_expression_heatmap <- shiny::renderCachedPlot(gene_expression_heatmap(),
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
      nrow = length(unique(filtered_gene_expressions()[["gene"]])),
      ncol = length(unique(filtered_gene_expressions()[["age"]]))
    )
  })

  output$gene_expression_heatmap_ui <- shiny::renderUI({
    shinycssloaders::withSpinner(shiny::plotOutput(ns("gene_expression_heatmap"),
      height = paste0(gene_expression_plot_dims()[["nrow"]] * 400, "px"),
      width = ifelse(gene_expression_plot_dims()[["ncol"]] == 1, "60%", "100%")
    ),
    color = "#D3DCEF"
    )
  })

  # Save output ----

  gene_expression_data_download <- shiny::reactive({
    filtered_gene_expressions()
  })

  save_name <- shiny::reactive({
    download_name("gene_expression_heatmap", input$gene, input$tissue)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = filtered_gene_expressions,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plotId = ns("gene_expression_heatmap"),
    data = filtered_gene_expressions,
    save_name = save_name
  )
}
