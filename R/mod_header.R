#' Header UI
#'
mod_header_ui <- function() {
  shiny::div(
    class = "container-fluid",
    shiny::div(
      id = "header",
      shiny::div(class = "magora-topbar"),
      shiny::div(
        class = "magora-brand",
        shiny::tags$a(
          class = "magora-brand", href = "https://sagebionetworks.org/",
          target = "_blank",
          title = "Sage Bionetworks",
          tags$img(
            src = "www/sage-bionetworks-logo.svg",
            height = 75,
            width = 300,
            alt = "Sage Bionetworks"
          )
        )
      )
    )
  )
}
