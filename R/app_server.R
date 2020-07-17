#' The application server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server <- function(input, output, session) {
  shiny::callModule(mod_pathology_server, "pathology")
  shiny::callModule(mod_gene_expression_server, "gene_expression")
}
