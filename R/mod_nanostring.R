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
      shiny::column(
        width = 12,
        shiny::plotOutput(ns("nanostring_plot"))
      )
    )
  )
}

#' Nanostring page server function
#'
#' @noRd
mod_nanostring_server <- function(input, output, session) {
  ns <- session$ns

  output$nanostring_plot <- shiny::renderPlot(
    magora_corrplot(magora::nanostring_for_plot)
  )
}

## To be copied in the UI
# mod_nanostring_ui("nanostring")

## To be copied in the server
# callModule(mod_nanostring_server, "nanostring")
