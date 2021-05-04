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
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(liveSearch = TRUE, liveSearchStyle = "startswith")
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

    shiny::validate(
      shiny::need(!is.null(input$gene), message = "Please select one or more genes.")
    )

    magora::gene_expressions %>%
      dplyr::filter(
        .data$gene %in% input$gene,
        .data$tissue == input$tissue
      )
  })

  # Generate plot ----

  gene_expression_heatmap <- shiny::reactive({

    shiny::validate(
      shiny::need(nrow(filtered_gene_expressions()) > 0, message = "No data available for the selected combination.")
    )

    filtered_gene_expressions() %>%
      dplyr::filter(!is.na(.data$padj)) %>%
      complete_gene_expression_heatmap_data(input$gene) %>%
      magora_heatmap()
  })

  output$gene_expression_heatmap <- shiny::renderCachedPlot({
    gene_expression_heatmap()
    },
    cacheKeyExpr = {
      list(
        input$gene,
        input$tissue
      )
    },
    res = 96
  )

  gene_expression_plot_dims <- shiny::reactive({
    list(
      nrow = length(unique(filtered_gene_expressions()[["gene"]])),
      ncol = length(unique(filtered_gene_expressions()[["age"]])) * length(unique(filtered_gene_expressions()[["sex"]])) * length(unique(filtered_gene_expressions()[["strain"]]))
    )
  })

  output$gene_expression_heatmap_ui <- shiny::renderUI({
    shinycssloaders::withSpinner(shiny::plotOutput(ns("gene_expression_heatmap"),
      height = paste0(200 + gene_expression_plot_dims()[["nrow"]] * 50, "px"),
      width = min(1000, paste0(150 + gene_expression_plot_dims()[["ncol"]] * 50, "px"))
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
