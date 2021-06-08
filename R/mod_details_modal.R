#' "More Details" Modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_details_modal_ui <- function(id){
  ns <- NS(id)
  shiny::actionLink(ns("details"), "More details")
}

#' "More Details" Modal  Server Function
#'
#' @noRd
mod_details_modal_server <- function(input, output, session){
  ns <- session$ns
  shiny::observeEvent(input$details, {
    browser()
    shiny::showModal(
      shiny::modalDialog(
        size = "m",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shiny::includeMarkdown(app_sys("app", "www", "content", ns, "details.md")),
      )
    )
  })
}

## To be copied in the UI
# mod_details_modal_ui("pathology")

## To be copied in the server
# callModule(mod_details_modal_server, "pathology")

