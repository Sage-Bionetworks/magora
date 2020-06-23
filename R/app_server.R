#' The application server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server <- function(input, output, session) {
  shiny::callModule(mod_pathology_server, "pathology")
}
