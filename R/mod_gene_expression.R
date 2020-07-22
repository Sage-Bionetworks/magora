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
        shinyWidgets::pickerInput(
          ns("gene_id"),
          "Select a gene",
          choices = unique(magora::gene_expressions[["gene_id"]])
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
        .data$gene_id %in% input$gene_id,
        .data$mouse_line %in% input$mouse_line
      )
  })

  output$gene_expression_plot <- shiny::renderPlot({
    shiny::req(nrow(filtered_gene_expressions()) > 0)

    filtered_gene_expressions() %>%
      dplyr::mutate(
        mouse_line = forcats::fct_drop(.data$mouse_line),
        mouse_line = forcats::fct_expand(.data$mouse_line, input$mouse_line),
        mouse_line = forcats::fct_relevel(.data$mouse_line, input$mouse_line)
      ) %>%
      magora_boxplot(plot_type = "gene expression")
  })
}

## To be copied in the UI
# mod_gene_expression_ui("gene_expression")

## To be copied in the server
# callModule(mod_gene_expression_server, "gene_expression")
