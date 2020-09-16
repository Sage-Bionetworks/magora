#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
run_app <- function(
                    ...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(... ,
                      gene_expressions = readRDS(system.file("extdata/gene_expressions.rds", package = "magora")))
  )
}
