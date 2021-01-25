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
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        shiny::includeMarkdown(app_sys("app", "www", "pathology_content.md")),
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("phenotype"),
            "Phenotype",
            choices = unique(magora::phenotypes[["phenotype"]])
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("mouse_line"),
            "Mouse lines",
            choices = as.character(levels(magora::phenotypes[["mouse_line"]])),
            multiple = TRUE,
            selected = c("C57BL6J", "5XFAD")
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = unique(magora::phenotypes[["tissue"]])
          )
        )
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 3,
          offset = 9,
          shiny::column(
            width = 6,
            mod_download_data_ui(ns("download_data"))
          ),

          shiny::column(
            width = 6,
            mod_download_plot_ui(ns("download_plot"))
          )
        )
      ),
      shiny::column(
        width = 12,
        align = "center",
        shiny::uiOutput(ns("phenotype_plot_ui"))
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

  output$phenotype_plot <- shiny::renderPlot(phenotype_plot(), res = 96)

  phenotype_plot_dims <- shiny::reactive({
    list(
      nrow = ceiling(length(input$mouse_line) / 2),
      ncol = ifelse(length(input$mouse_line) == 1, 1, 2)
    )
  })

  output$phenotype_plot_ui <- shiny::renderUI({

    # Validating mouse line input twice, otherwise there's a quartz error in computing the plot height below
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    shinycssloaders::withSpinner(shiny::plotOutput(ns("phenotype_plot"),
      height = paste0(phenotype_plot_dims()[["nrow"]] * 400, "px"),
      width = ifelse(phenotype_plot_dims()[["ncol"]] == 1, "60%", "100%")
    ),
    color = "#D3DCEF"
    )
  })

  # Save output ----

  phenotype_data_download <- shiny::reactive({
    filtered_phenotypes() %>%
      dplyr::select(.data$mouse_line, .data$tissue, .data$age, .data$sex, .data$phenotype, .data$value) %>%
      dplyr::arrange(.data$mouse_line, .data$tissue, .data$age, .data$sex) %>%
      dplyr::rename_all(function(x) stringr::str_to_title(stringr::str_replace_all(x, "_", " ")))
  })

  save_name <- shiny::reactive({
    download_name("phenotype", input$phenotype, input$mouse_line, input$tissue)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = phenotype_data_download,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plot = phenotype_plot,
    data = phenotype_data_download,
    save_name = save_name,
    plot_dims = phenotype_plot_dims
  )
}
