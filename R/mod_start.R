#' Start page UI Function
#'
mod_start_ui <- function() {
  shiny::tabPanel(
    "Start",
    shiny::div(
      class = "magora-page",
      shiny::div(
        class = "start-banner",
        shiny::div(class = "start-banner-img"),
        shiny::div(class = "start-title", shiny::h1("MODEL-AD Mouse Explorer")),
      ),
      shiny::includeMarkdown(app_sys("app", "www", "content", "start", "content.md"))
    )
  )
}
