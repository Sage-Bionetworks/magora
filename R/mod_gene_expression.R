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
          choices = sort(magora::gene_expressions_genes),
          multiple = FALSE,
          selectize = TRUE
        ),
        shinyWidgets::pickerInput(
          ns("mouse_line"),
          "Select mouse lines",
          choices = sort(magora::gene_expressions_mouse_lines),
          multiple = TRUE,
          selected = c("5XFAD", "C57BL6J")
        ),
        shinyWidgets::pickerInput(
          ns("tissue"),
          "Select tissue",
          choices = sort(magora::gene_expressions_tissue)
        )
      ),
      shiny::uiOutput(ns("gene_expression_plot_ui"))
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_server <- function(input, output, session) {
  ns <- session$ns

  gene_expressions <- golem::get_golem_options("gene_expressions")

  filtered_gene_expressions <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    gene_expressions %>%
      dplyr::filter(
        .data$gene %in% input$gene,
        .data$mouse_line %in% input$mouse_line,
        .data$tissue %in% input$tissue
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

  output$gene_expression_plot_ui <- shiny::renderUI({
    shiny::mainPanel(
      width = 9,
      shiny::plotOutput(ns("gene_expression_plot"),
        height = glue::glue("{ceiling(length(input$mouse_line)/2)*400}px")
      )
    )
  })
}

## To be copied in the UI
# mod_gene_expression_ui("gene_expression")

## To be copied in the server
# callModule(mod_gene_expression_server, "gene_expression")
