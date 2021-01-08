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

download_plot_data <- function(plot, data, name, height = 5, width = 10) {
  shiny::downloadHandler(
    filename = function() {
      glue::glue("{name}.zip")
    },
    content = function(file) {
      plot_file <- glue::glue("{name}_plot.png")
      ggplot2::ggsave(filename = plot_file, plot = plot, width = width, height = height, units = "in", dpi = 300)

      data_file <- glue::glue("{name}_data.csv")
      readr::write_csv(data, path = data_file)

      utils::zip(file, files = c(data_file, plot_file))

      fs::file_delete(plot_file)
      fs::file_delete(data_file)
    }
  )
}
