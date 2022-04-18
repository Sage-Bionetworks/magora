#' Start page UI Function
#'
mod_start_ui <- function() {

  shiny::tabPanel(
    "Start",
    shiny::div(class = "magora-hr"),
    shiny::div(
      class = "magora-page",
      shiny::div(
        class = "start-banner",
        shiny::div(class = "start-banner-img"),
        shiny::div(
          class = "start-title", shiny::includeMarkdown(app_sys("app", "www", "content", "start", "title.md"))
        ),
        shiny::br(),
        shiny::br(),
        shiny::div(
          class = "start-header",
          shiny::includeMarkdown(app_sys("app", "www", "content", "start", "header.md"))
        )
      ),
      shiny::includeMarkdown(app_sys("app", "www", "content", "start", "content.md"))
    )
  )
}
