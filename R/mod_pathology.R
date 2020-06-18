#' Pathology page UI function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput
mod_pathology_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Pathology",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        pickerInput(
          ns("phenotype"),
          "Select a phenotype",
          choices = unique(phenotypes[["Phenotype"]])
        ),
        pickerInput(
          ns("mouse_line"),
          "Select mouse lines",
          choices = unique(phenotypes[["mouse_line"]]),
          multiple = TRUE,
          selected = "BL6"
        ),
        pickerInput(
          ns("tissue"),
          "Select tissue",
          choices = unique(phenotypes[["Tissue"]])
        )
      ),
      mainPanel(
        width = 9,
        plotOutput(ns("phenotype_plot"))
      )
    )
  )
}

#' Pathology page server function
#'
#' @noRd
mod_pathology_server <- function(input, output, session) {
  ns <- session$ns

  filtered_phenotypes <- reactive({
    validate(
      need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )
    phenotypes %>%
      filter_phenotypes(input$phenotype, input$mouse_line, input$tissue)
  })

  output$phenotype_plot <- renderPlot({
    plot_phenotypes(filtered_phenotypes(), input$phenotype)
  })
}

## To be copied in the UI
# mod_pathology_ui("pathology")

## To be copied in the server
# callModule(mod_pathology_server, "pathology")
