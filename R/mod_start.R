#' Start page UI Function
#'
mod_start_ui <- function() {
  shiny::tabPanel(
    "Start",
    shiny::div(
      class = "frontp",
      shiny::div(
        class = "front-banner",
        shiny::div(class = "imgcon"),
        shiny::div(class = "hcon", shiny::h1("Mouse Agora")),
      ),
      shiny::tags$p(class = "intro", shinipsum::random_text(nwords = 50)),
      shiny::div(class = "intro-divider"),
      shiny::tags$p(shinipsum::random_text(nwords = 30)),
      shiny::div(
        class = "box-con",
        shiny::tags$a(
          target = "_blank",
          href = "https://sagebionetworks.org/",
          shiny::div(
            class = "float box box-more",
            shiny::tags$p(class = "intro", shinipsum::random_text(nwords = 2)),
            shiny::tags$p(shinipsum::random_text(nwords = 10))
          )
        ),
        shiny::tags$a(
          target = "_blank",
          href = "https://sagebionetworks.org/",
          shiny::div(
            class = "float box box-rear",
            shiny::tags$p(
              shiny::tags$img(class = "rear-preview", src = "www/MODEL AD.png"),
              shinipsum::random_text(nwords = 40)
            )
          )
        )
      ),
      shiny::div(
        class = "box box-timeout",
        shiny::tags$p(
          shinipsum::random_text(nwords = 20)
        )
      )
    )
  )
}
