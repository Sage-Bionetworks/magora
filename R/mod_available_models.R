#' available_models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_available_models_ui <- function(id) {
  ns <- NS(id)
  title <- "Available Models and Data"

  shiny::tabPanel(
    title,
    value = title,
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        reactable::reactable(magora::available_models,
          columns = list(
            `Source Data` = reactable::colDef(cell = function(value) {
              htmltools::tags$a(href = value, target = "_blank", "View data")
            }),
            `Model Information` = reactable::colDef(cell = function(value) {
              htmltools::tags$a(href = value, target = "_blank", "Model details")
            }),
            Pathology = colDef(cell = function(value) {
              ifelse(is.na(value), "", "\u2713")
            }),
            `Gene Expression` = colDef(cell = function(value) {
              ifelse(is.na(value), "", "\u2713")
            }),
            `Correlation with Human Disease` = colDef(cell = function(value) {
              ifelse(is.na(value), "", "\u2713")
            })
          ),
          sortable = FALSE,
          pagination = FALSE
        )
      )
    )
  )
}

#' available_models Server Function
#'
#' @noRd
mod_available_models_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_available_models_ui("available_models")

## To be copied in the server
# callModule(mod_available_models_server, "available_models")
