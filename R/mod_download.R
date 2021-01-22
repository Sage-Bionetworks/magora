# Data ----

#' Download Data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_download_data_ui <- function(id) {
  ns <- shiny::NS(id)
  magora_download_button(ns("download_data"), "Save data")
}

#' Download Data Server Function
#'
#' @noRd
mod_download_data_server <- function(input, output, session, data, save_name) {
  ns <- session$ns

  # Only enable button if there is data available
  shiny::observe({
    shinyjs::toggleState(id = "download_data", condition = nrow(data()) > 0)
  })

  output$download_data <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{save_name()}_data.csv")
    },
    content = function(file) {
      readr::write_csv(data(), path = file)
    }
  )
}

# Plot ----

#' Download Plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_download_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  magora_download_button(ns("download_plot"), "Save plot")
}

#' Download Data Server Function
#'
#' @noRd
mod_download_plot_server <- function(input, output, session, plot, data, save_name, plot_dims) {
  ns <- session$ns

  # Only enable button if there is data available
  shiny::observe({
    shinyjs::toggleState(id = "download_plot", condition = nrow(data()) > 0)
  })

  save_dims <- shiny::reactive({
    list(
      height = plot_dims()[["nrow"]] * 5,
      width = ifelse(plot_dims()[["ncol"]] == 1, 6, plot_dims()[["ncol"]] * 5)
    )
  })

  output$download_plot <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{save_name()}_plot.png")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file, plot = plot(),
        height = save_dims()[["height"]], width = save_dims()[["width"]],
        units = "in", dpi = 300
      )
    }
  )
}

# Utils ----

# Create a download button with a different icon
magora_download_button <- function(outputId, label = "Download", class = NULL, ...) {
  shiny::tags$a(
    id = outputId,
    class = paste("btn btn-default shiny-download-link", class),
    href = "", target = "_blank", download = NA, shiny::icon("download", lib = "glyphicon"), label, ...
  )
}

download_name <- function(data_type = c("phenotype", "gene_expression"), ...) {
  data_type <- match.arg(data_type)

  data_type_name <- switch(data_type,
    phenotype = "Pathology",
    gene_expression = "Gene_Expression"
  )

  # Separate everything in ... by _, and collapse each with _ (in case of multiples)
  # Sanitize name to remove any invalid characters or e.g. / which would create a subdir
  fs::path_sanitize(
    glue::glue("{data_type_name}_{paste0(c(...), collapse = '_')}")
  )
}
