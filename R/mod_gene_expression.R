#' Gene Expression page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_ui <- function(id) {
  ns <- NS(id)
  shiny::tabPanel(
    "Gene Expression",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shinyWidgets::pickerInput(
          ns("gene_id"),
          "Select a gene",
          choices = unique(magora::gene_expressions[["gene_id"]])
        ),
        shinyWidgets::pickerInput(
          ns("mouse_line"),
          "Select mouse lines",
          choices = unique(magora::gene_expressions[["mouse_line"]]),
          multiple = TRUE,
          selected = c("5XFAD", "C57BL6J")
        ),
        shinyWidgets::pickerInput(
          ns("tissue"),
          "Select tissue",
          choices = NULL
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::plotOutput(ns("gene_expression_plot"))
      )
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_server <- function(input, output, session) {
  ns <- session$ns

  filtered_gene_expressions <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    magora::gene_expressions %>%
      dplyr::filter(
        .data$gene_id %in% input$gene_id,
        .data$mouse_line %in% input$mouse_line
      )
  })

  output$gene_expression_plot <- shiny::renderPlot({
    shiny::req(nrow(filtered_gene_expressions()) > 0)

    filtered_gene_expressions() %>%
      plot_gene_expressions()
  })
}

## To be copied in the UI
# mod_gene_expression_ui("gene_expression")

## To be copied in the server
# callModule(mod_gene_expression_server, "gene_expression")
