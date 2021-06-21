#' The application server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server <- function(input, output, session) {

  # Check for page to go to from URL
  shiny::observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query$page)) {
      shiny::updateTabsetPanel(session, inputId = "page", selected = query$page)
    }
  })

  shiny::callModule(mod_pathology_server, "pathology")

  shiny::callModule(mod_gene_expression_volcano_server, "gene_expression_volcano")
  shiny::callModule(mod_gene_expression_heatmap_server, "gene_expression_heatmap")

  shiny::callModule(mod_nanostring_server, "nanostring")

  shiny::callModule(mod_available_models_server, "available_models")
}
