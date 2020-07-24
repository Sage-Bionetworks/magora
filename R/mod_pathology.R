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
          ns("mouse_line_group"),
          "Select mouse lines",
          choices = unique(magora::phenotypes[["mouse_line_group"]]),
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

  shiny::observeEvent(input$phenotype, {
    phenotype_data <- dplyr::filter(magora::phenotypes, .data$phenotype == input$phenotype)
    available_tissue <- unique(phenotype_data[["tissue"]])

    shinyWidgets::updatePickerInput(
      session = session,
      "tissue",
      choices = available_tissue
    )
  })

  filtered_phenotypes <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_line_group), message = "Please select one or more mouse lines.")
    )

    magora::phenotypes %>%
      dplyr::filter(
        .data$phenotype %in% input$phenotype,
        .data$mouse_line_group %in% input$mouse_line_group,
        .data$tissue %in% input$tissue
      )
  })

  output$phenotype_plot <- shiny::renderPlot({
    shiny::req(nrow(filtered_phenotypes()) > 0)

    filtered_phenotypes() %>%
      expand_mouse_line_factor_from_group(input$mouse_line_group) %>%
      magora_boxplot(plot_type = "phenotype")
  })
}

## To be copied in the UI
# mod_pathology_ui("pathology")

## To be copied in the server
# callModule(mod_pathology_server, "pathology")
