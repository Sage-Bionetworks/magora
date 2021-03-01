#' The application server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server <- function(input, output, session) {
  shiny::callModule(mod_pathology_server, "pathology")

  gene_expressions <- golem::get_golem_options("gene_expressions")
  shiny::callModule(mod_gene_expression_server, "gene_expression", gene_expressions)

  shiny::callModule(mod_gene_expression_volcano_server, "gene_expression_volcano")

  shiny::callModule(mod_nanostring_server, "nanostring")
}
