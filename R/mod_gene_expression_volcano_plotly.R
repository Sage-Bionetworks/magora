#' Gene Expression Volcano page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_volcano_plotly_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Gene Expression (Interactive)"

  shiny::tabPanel(
    title,
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        "Please select a strain, tissue, sex, and age from the dropdown lists.",
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("strain"),
            "Strain",
            choices = unique(magora::gene_expressions_for_volcano[["strain"]]),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = unique(magora::gene_expressions_for_volcano[["tissue"]]),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("sex"),
            "Sex",
            choices = unique(magora::gene_expressions_for_volcano[["sex"]]),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("age"),
            "Age",
            choices = unique(magora::gene_expressions_for_volcano[["age"]]),
            multiple = FALSE
          )
        ),
      ),
      # shiny::fluidRow(
      #   class = "magora-row",
      #   shiny::column(
      #     width = 3,
      #     offset = 9,
      #     shiny::column(
      #       width = 6,
      #       mod_download_data_ui(ns("download_data"))
      #     ),
      #
      #     shiny::column(
      #       width = 6,
      #       mod_download_plot_ui(ns("download_plot"))
      #     )
      #   ),
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
mod_gene_expression_volcano_plotly_server <- function(input, output, session, gene_expressions) {
  ns <- session$ns

  # Filter data based on inputs ----

  filtered_gene_expressions <- shiny::reactive({
    magora::gene_expressions_for_volcano %>%
      dplyr::filter(
        .data$strain == input$strain,
        .data$tissue == input$tissue,
        .data$sex == input$sex,
        .data$age == input$age
      )
  })

  # Generate plot ----

  gene_expression_plot <- shiny::reactive({
    shiny::validate(
      shiny::need(nrow(filtered_gene_expressions()) > 0, message = "There is no data for the selected combination.")
    )

    filtered_gene_expressions() %>%
      magora_volcano_plotly()
  })

  output$gene_expression_plot <- plotly::renderPlotly(gene_expression_plot()) # , res = 96)

  gene_expression_plot_dims <- shiny::reactive({
    list(
      nrow = 1.5,
      ncol = 2
    )
  })

  output$gene_expression_plot_ui <- shiny::renderUI({
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("gene_expression_plot"),
      height = paste0(gene_expression_plot_dims()[["nrow"]] * 400, "px"),
      width = ifelse(gene_expression_plot_dims()[["ncol"]] == 1, "60%", "100%")
    ),
    color = "#D3DCEF"
    )
  })
}
