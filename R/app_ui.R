#' The application User Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    magoraPage(
      id = "page",
      header = mod_header_ui(),
      mod_start_ui(),
      mod_pathology_ui("pathology"),
      shiny::navbarMenu(
        title = "Gene Expression",
        mod_gene_expression_volcano_ui("gene_expression_volcano"),
        mod_gene_expression_heatmap_ui("gene_expression_heatmap")
      ),
      mod_nanostring_ui("nanostring")
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
    golem::favicon(ext = "png"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "MODEL-AD Mouse Explorer"
    )
  )
}
