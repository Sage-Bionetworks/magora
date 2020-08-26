#' Gene Expression page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "Gene Expression",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput(
          ns("gene"),
          "Select a gene",
          choices = unique(magora::gene_expressions[["gene"]]),
          multiple = FALSE,
          selectize = TRUE
        ),
        shinyWidgets::pickerInput(
          ns("mouse_line"),
          "Select mouse lines",
          choices = as.character(levels(magora::gene_expressions[["mouse_line"]])),
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
        .data$gene %in% input$gene,
        .data$mouse_line %in% input$mouse_line
      )
  })

  output$gene_expression_plot <- shiny::renderPlot({
    shiny::validate(
      shiny::need(nrow(filtered_gene_expressions()) > 0, message = "There is no data for the selected combination.")
    )

    filtered_gene_expressions() %>%
      expand_mouse_line_factor_from_selection(input$mouse_line) %>%
      magora_boxplot(plot_type = "gene expression")
  })
}

## To be copied in the UI
# mod_gene_expression_ui("gene_expression")

## To be copied in the server
# callModule(mod_gene_expression_server, "gene_expression")
