#' Pathology page UI function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_pathology_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Pathology"

  shiny::tabPanel(
    title,
    shiny::div(
      shiny::h3(class = "tabTitle", title),
      shiny::tags$p(class = "tabDesc", shinipsum::random_text(nwords = 10)),
      shiny::hr()
    ),
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
          choices = as.character(levels(magora::phenotypes[["mouse_line"]])),
          multiple = TRUE,
          selected = c("C57BL6J", "5XFAD")
        ),
        shinyWidgets::pickerInput(
          ns("tissue"),
          "Select tissue",
          choices = unique(magora::phenotypes[["tissue"]])
        )
      ),
      shiny::mainPanel(
        width = 9,
        shinycssloaders::withSpinner(shiny::plotOutput(ns("phenotype_plot")))
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
    available_tissue <- magora::phenotype_tissue[[input$phenotype]]

    # If the tissue previously selected is still available, keep it selected
    selected_tissue <- ifelse(input$tissue %in% available_tissue, input$tissue, available_tissue[[1]])

    shinyWidgets::updatePickerInput(
      session = session,
      "tissue",
      choices = available_tissue,
      selected = selected_tissue
    )
  })

  filtered_phenotypes <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    magora::phenotypes %>%
      dplyr::filter(
        .data$phenotype %in% input$phenotype,
        .data$mouse_line %in% input$mouse_line,
        .data$tissue %in% input$tissue
      )
  })

  output$phenotype_plot <- shiny::renderPlot({
    shiny::req(input$tissue %in% magora::phenotype_tissue[[input$phenotype]])

    shiny::validate(
      shiny::need(nrow(filtered_phenotypes()) > 0, message = "There is no data for the selected combination.")
    )

    filtered_phenotypes() %>%
      expand_mouse_line_factor_from_selection(input$mouse_line) %>%
      magora_boxplot(plot_type = "phenotype")
  })
}

## To be copied in the UI
# mod_pathology_ui("pathology")

## To be copied in the server
# callModule(mod_pathology_server, "pathology")
