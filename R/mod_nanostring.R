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
      shiny::h3(class = "tab-title", title),
      shiny::includeMarkdown(app_sys("app", "www", "nanostring_content.md")),
      shiny::hr(),
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
        shiny::plotOutput(ns("nanostring_plot"), width = "1000px", height = "666px")
      )
    )
  )
}

#' Nanostring page server function
#'
#' @noRd
mod_nanostring_server <- function(input, output, session) {
  ns <- session$ns

  nanostring_plot <- shiny::reactive({
    magora_corrplot(magora::nanostring_for_plot)
  })

  output$nanostring_plot <- shiny::renderPlot(nanostring_plot(), res = 96)

  # Save output ----

  # The save functions expect a reactive data set, name, and dimensions (for the other pages)
  # Just make into reactives for consistent usage
  nanostring_data <- shiny::reactive(magora::nanostring)
  nanostring_name <- shiny::reactive(
    "nanostring"
  )
  # Save size will be height = nrow * 5, width = ncol * 5
  nanostring_plot_dims <- shiny::reactive({
    list(nrow = 1.5, ncol = 2.5)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = nanostring_data,
    save_name = nanostring_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plot = nanostring_plot,
    data = nanostring_data,
    save_name = nanostring_name,
    plot_dims = nanostring_plot_dims
  )
}

## To be copied in the UI
# mod_nanostring_ui("nanostring")

## To be copied in the server
# callModule(mod_nanostring_server, "nanostring")
