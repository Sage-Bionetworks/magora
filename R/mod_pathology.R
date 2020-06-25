#' Pathology page UI function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_pathology_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "Pathology",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shinyWidgets::pickerInput(
          ns("phenotype"),
          "Select a phenotype",
          choices = unique(magora::phenotypes[["phenotype"]])
        ),
        shinyWidgets::pickerInput(
          ns("mouse_line"),
          "Select mouse lines",
          choices = unique(magora::phenotypes[["mouse_line"]]),
          multiple = TRUE,
          selected = "BL6"
        ),
        shinyWidgets::pickerInput(
          ns("tissue"),
          "Select tissue",
          choices = unique(magora::phenotypes[["tissue"]])
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::plotOutput(ns("phenotype_plot"))
      )
    )
  )
}

#' Pathology page server function
#'
#' @noRd
mod_pathology_server <- function(input, output, session) {
  ns <- session$ns

  filtered_phenotypes <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    filter_pathology(magora::phenotypes, input$phenotype, input$mouse_line, input$tissue)
  })

  output$phenotype_plot <- shiny::renderPlot({
    filtered_phenotypes() %>%
      expand_mouse_line_factor(input$mouse_line) %>%
      plot_phenotypes()
  })
}

## To be copied in the UI
# mod_pathology_ui("pathology")

## To be copied in the server
# callModule(mod_pathology_server, "pathology")
