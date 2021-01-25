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
      shiny::p(class = "tab-description", "This explorer shows the extent to which gene expression changes in MODEL-AD mouse models mimic expression changes in humans with AD."),
      shiny::p(class = "tab-description", "In this analysis, gene expression changes in each mouse model are calculated compared to controls. The resulting fold-change for each gene is then correlated with the fold-change in human AD vs control. See this", shiny::a(href = "https://molecularneurodegeneration.biomedcentral.com/articles/10.1186/s13024-020-00412-5", target = "_blank", "publication by Preuss et al"), "for more detail."),
      shiny::p(class = "tab-description", "The correlations between mouse and human for each gene are then mapped to consensus network modules from a harmonized co-expression analysis based on 3 human postmortem brain cohorts. See this", shiny::a(href = "https://www.cell.com/cell-reports/fulltext/S2211-1247(20)30889-5", target = "_blank", "publication by Wan et al"), "for more detail."),
      shiny::p(class = "tab-description", "BLUE depicts a positive correlation, in which the gene expression changes in the mouse model are similar to changes in humans. RED depicts a negative correlation, in which the gene expression changes in the mouse model are in the opposite direction of humans."),
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
        shiny::plotOutput(ns("nanostring_plot"), width = "100%", height = "600px")
      )
    )
  )
}

#' Nanostring page server function
#'
#' @noRd
mod_nanostring_server <- function(input, output, session) {
  ns <- session$ns

  nanostring_plot <- reactive({
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
  nanostring_plot_dims <- shiny::reactive({
    list(nrow = 1.5, ncol = 2)
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
