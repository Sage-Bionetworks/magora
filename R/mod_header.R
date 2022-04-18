#' Header UI
#'
mod_header_ui <- function() {
  shiny::div(
    class = "container-fluid",
    shiny::div(class = "magora-topbar"),
    shiny::div(
      class = "magora-brand",
      shiny::tags$a(
        title = "Model AD Explorer",
        shiny::tags$img(
          src = "www/full_logo.svg",
          width = "278px",
          height = "100px",
          alt = "Model AD Explorer"
        )
      )
    )
  )
}
