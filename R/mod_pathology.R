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
      shiny::h3(class = "tab-title", title),
      shiny::tags$p(class = "tab-description", "This explorer shows changes in pathological hallmarks associated with AD."),
      shiny::tags$p(class = "tab-description", "Measurements include amyloid beta (Ab) levels in brain and plasma as well as counts of neurons and glia from immunofluorescent stains."),
      shiny::tags$p(class = "tab-description", "Please select a phenotype, mouse line, and tissue from the dropdown lists."),
      shiny::hr()
    ),
    shiny::fluidRow(
      class = "magora-page",
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("phenotype"),
            "Phenotype",
            choices = unique(magora::phenotypes[["phenotype"]])
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("mouse_line"),
            "Mouse lines",
            choices = as.character(levels(magora::phenotypes[["mouse_line"]])),
            multiple = TRUE,
            selected = c("C57BL6J", "5XFAD")
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = unique(magora::phenotypes[["tissue"]])
          )
        ),
        shiny::column(
          width = 3,
          shiny::downloadButton(ns("download_plot_data"), "Download plot and data")
        )
      ),
      shiny::column(
        width = 12,
        shinycssloaders::withSpinner(shiny::plotOutput(ns("phenotype_plot")),
          color = "#D3DCEF"
        )
      )
    )
  )
}

#' Pathology page server function
#'
#' @noRd
mod_pathology_server <- function(input, output, session) {
  ns <- session$ns

  # Update tissue options available based on phenotype selected -----

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

  # Filter data based on inputs ----

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

  # Generate plot ----

  phenotype_plot <- shiny::reactive({
    shiny::req(input$tissue %in% magora::phenotype_tissue[[input$phenotype]])

    shiny::validate(
      shiny::need(nrow(filtered_phenotypes()) > 0, message = "There is no data for the selected combination.")
    )

    filtered_phenotypes() %>%
      expand_mouse_line_factor_from_selection(input$mouse_line) %>%
      magora_boxplot(plot_type = "phenotype")
  })

  output$phenotype_plot <- shiny::renderPlot(phenotype_plot())

  # Save output ----

  save_name <- reactive({
    glue::glue("Pathology_{input$phenotype}_{paste0(input$mouse_line, collapse = '_')}_{input$tissue}")
  })

  output$download_plot_data <- download_plot_data(
    plot = phenotype_plot(),
    data = filtered_phenotypes() %>%
      dplyr::select(mouse_line, tissue, age, sex, phenotype, value) %>%
      dplyr::arrange(mouse_line, age, sex),
    name = save_name()
  )
}
