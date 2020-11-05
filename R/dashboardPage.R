dashboardPage <-
  ## Modified navbarPage from shiny
  ## Cuts bloat and enables use of tags$head with `thead`
  function(..., id = "dashboard", thead = NULL, header = NULL, footer = NULL, windowTitle = title) {
    navbarClass <- "navbar navbar-default"
    tabs <- list(...)
    tabset <- shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id)
    containerDiv <- shiny::div(class = "container", tabset$navList)
    contentDiv <- shiny::div(class = "container-fluid")
    if (!is.null(header)) {
      contentDiv <- shiny::tagAppendChild(contentDiv, shiny::div(class = "row", header))
    }
    contentDiv <- shiny::tagAppendChild(contentDiv, tabset$content)
    if (!is.null(footer)) {
      contentDiv <- shiny::tagAppendChild(contentDiv, shiny::div(class = "row", footer))
    }
    shiny::bootstrapPage(
      thead,
      shiny::tags$nav(class = navbarClass, role = "navigation", containerDiv),
      contentDiv
    )
  }
