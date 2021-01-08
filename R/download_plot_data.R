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
