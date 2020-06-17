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
    phenotypes %>%
      dplyr::filter(
        .data$Tissue == input$tissue,
        .data$mouse_line %in% input$mouse_line,
        .data$Phenotype == input$phenotype,
        !is.na(.data$value)
      )
  })

  output$phenotype_plot <- renderPlot({
    ggplot2::ggplot(filtered_phenotypes(), ggplot2::aes(x = .data$Age, y = .data$value, fill = .data$Sex)) +
      ggplot2::geom_boxplot(position = ggplot2::position_dodge2(width = 0.5, preserve = "single")) +
      ggplot2::facet_wrap(ggplot2::vars(.data$`Mouse Line`)) +
      ggplot2::labs(x = "Age", y = input$phenotype) +
      sagethemes::scale_fill_sage_d() +
      sagethemes::theme_sage()
  })
}

## To be copied in the UI
# mod_pathology_ui("pathology")

## To be copied in the server
# callModule(mod_pathology_server, "pathology")
