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
          class = "magora-brand", href = "https://www.model-ad.org/",
          target = "_blank",
          title = "MODEL-AD",
          shiny::tags$img(
            src = "www/model-ad-logo-1.jpg",
            width = "290px",
            height = "100px",
            alt = "MODEL-AD"
          )
        )
      )
    )
  )
}
