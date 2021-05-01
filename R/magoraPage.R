#' Create a page with a header and top level navigation bar
#'
#' @description Create a page that contains a header as well as a top level navigation bar that can be used to toggle a set of tab elements.
#'
#' @param header Page header.
#' @param ... Tab elements to include in the page.
#'
#' @noRd
magoraPage <- function(header, ...) {

  # Construct tabs (navbar and actual content) based on inputs
  tabs <- shiny:::buildTabset(tabs = list(...), ulClass = "nav navbar-nav")

  # Create div for navbar
  div_navbar_container <- shiny::div(class = "navbar-container", tabs$navList)

  # Create div for content (actual pages)
  div_content <- shiny::div(class = "container-fluid", tabs$content)

  # Create page
  shiny::bootstrapPage(
    header,
    shiny::tags$nav(class = "navbar navbar-default", role = "navigation", div_navbar_container),
    shiny::hr(),
    div_content
  )
}
