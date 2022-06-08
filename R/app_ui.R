#' The application User Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML(
      'function link(page) {
      Shiny.onInputChange("page_link", page)
      }'
    )),
    golem_add_external_resources(),
    mod_header_ui(),
    shiny::navbarPage(
      id = "page",
      title = NULL,
      header = NULL,
      collapsible = TRUE,
      mod_start_ui(),
      mod_pathology_ui("pathology"),
      shiny::navbarMenu(
        title = "Gene Expression",
        mod_gene_expression_volcano_ui("gene_expression_volcano"),
        mod_gene_expression_heatmap_ui("gene_expression_heatmap")
      ),
      mod_nanostring_ui("nanostring"),
      mod_available_models_ui("available_models")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www", app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(ext = "ico"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Model AD Explorer"
    )
  )
}
