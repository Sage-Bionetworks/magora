#' The application server
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  callModule(mod_pathology_server, "pathology")
}
