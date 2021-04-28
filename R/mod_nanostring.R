#' Nanostring page UI function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_nanostring_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Correlation with Human Disease"

  shiny::tabPanel(
    title,
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        shiny::includeMarkdown(app_sys("app", "www", "nanostring_content.md")),
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("strain"),
            "Strain",
            choices = sort(unique(magora::nanostring_for_plot[["model"]])),
            selected = sort(unique(magora::nanostring_for_plot[["model"]])),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        ),
        shiny::column(
          width = 2,
          class = "dropdown-too-small",
          shinyWidgets::pickerInput(
            ns("sex"),
            "Sex",
            choices = sort(unique(magora::nanostring_for_plot[["sex"]])),
            selected = sort(unique(magora::nanostring_for_plot[["sex"]])),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE, container = "body")
          )
        ),
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            ns("age"),
            "Age",
            choices = levels(magora::nanostring_for_plot[["age_group"]]),
            selected = levels(magora::nanostring_for_plot[["age_group"]]),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        ),
        shiny::column(
          width = 2,
          style = "margin-top: 27.85px;",
          mod_download_data_ui(ns("download_data"))
        ),
        shiny::column(
          width = 2,
          style = "margin-top: 27.85px;",
          mod_download_plot_ui(ns("download_plot"))
        ),
      ),
      shiny::column(
        width = 12,
        shiny::uiOutput(ns("nanostring_plot_ui"))
      )
    )
  )
}

#' Nanostring page server function
#'
#' @noRd
mod_nanostring_server <- function(input, output, session) {
  ns <- session$ns


  # Filter data based on inputs ----

  filtered_nanostring <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$strain) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more models, sexes, and age groups.")
    )

    magora::nanostring %>%
      dplyr::filter(
        .data$model %in% input$strain,
        .data$sex %in% input$sex,
        .data$age_group %in% input$age
      )
  })

  filtered_nanostring_for_plot <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$strain) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more models, sexes, and age groups.")
    )

    magora::nanostring_for_plot %>%
      dplyr::filter(
        .data$model %in% input$strain,
        .data$sex %in% input$sex,
        .data$age_group %in% input$age
      )
  })

  # Generate plot ----

  nanostring_plot <- shiny::reactive({
    magora_corrplot(filtered_nanostring_for_plot())
  })

  output$nanostring_plot <- shiny::renderPlot(nanostring_plot(), res = 96)

  # Save size will be height = nrow * 5, width = ncol * 5
  nanostring_plot_dims <- shiny::reactive({
    list(
      nrow = length(input$strain) * length(input$sex) * length(input$age) + length(input$age),
      ncol = 2.5
    )
  })

  output$nanostring_plot_ui <- shiny::renderUI({

    # Validating mouse line input twice, otherwise there's a quartz error in computing the plot height below
    shiny::validate(
      shiny::need(!is.null(input$strain) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more models, sexes, and age groups.")
    )

    min_height <- 150 + 75 + 100 * length(input$age) # top labels, legend, each side facet label
    rows_height <- 150 + 75 + nanostring_plot_dims()[["nrow"]] * 25
    plot_height <- max(min_height, rows_height)

    shinycssloaders::withSpinner(
      shiny::plotOutput(
        ns("nanostring_plot"),
        height = paste0(plot_height, "px"),
        width = "1000px"
      ),
      color = "#D3DCEF"
    )
  })

  # Save output ----

  save_name <- shiny::reactive({
    download_name("nanostring", input$strain, input$sex, input$age)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = filtered_nanostring,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plotId = ns("nanostring_plot"),
    data = filtered_nanostring,
    save_name = save_name
  )
}

## To be copied in the UI
# mod_nanostring_ui("nanostring")

## To be copied in the server
# callModule(mod_nanostring_server, "nanostring")
