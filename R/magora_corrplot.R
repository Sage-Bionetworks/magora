#' Correlation plot for nanostring versus AMPAD data
#'
#' Creates a correlation plot comparing nanostring data to human genetic data, faceted by age group. Combines multiple plots (created via \link{single_age_corrplot}), one for each age of the mice in the sample.
#'
#' @param data Input data (\code{\link{nanostring_for_plot}}
#'
#' @return A ggplot2 object.
magora_corrplot <- function(data) {

  data <- data %>%
    dplyr::mutate(cluster_label = as.character(cluster),
                  cluster_label = dplyr::case_when(stringr::str_detect(cluster_label, "A \\(|B \\(|C \\(|D \\(") ~ stringr::str_replace(cluster_label, " \\(", "\n\\("),
                                                  TRUE ~ "Consensus Cluster E\n(Organelle Biogensis,\nCellular stress response)"),
                  cluster_label = forcats::fct_reorder(cluster_label, as.numeric(cluster)))

  data_by_age <- split(data, data$age_group)

  # Calculate the relative size of each plot by counting the number of modules in each

  plot_length <- sapply(data_by_age, FUN = function(x) unique(x[["n_rows"]]))

  plot_total_length <- sum(plot_length)

  plot_props <- plot_length / plot_total_length

  lapply(seq_along(data_by_age),
         function(x) {
           if (x == 1) {
             single_age_corrplot(data_by_age[[x]])
           } else {
             single_age_corrplot(data_by_age[[x]]) +
               ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                              strip.text.x = ggplot2::element_blank())
           }
         }) %>%
    patchwork::wrap_plots(guides = "collect") +
    patchwork::plot_layout(ncol = 1, heights = plot_props)
}

#' Correlation plot for nanostring versus AMPAD data, for a single age
#'
#' Creates a correlation plot comparing nanostring data to human genetic data, *only* for mice with a single age (e.g. all mice that are 4 months old). Multiple plots are then combined and displayed via \link{magora_corrplot}.
#'
#' @param data Input data (\code{\link{nanostring_for_plot}}, split or filtered by a single value of  \code{age_months}.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples {
#' nanostring_for_plot %>%
#'   dplyr::filter(age_group == "4 - 5 Months") %>%
#'   single_age_corrplot()
#' }
single_age_corrplot <- function(data) {

  ggplot2::ggplot() +
    ggplot2::geom_tile(data = data, ggplot2::aes(x = module, y = model_sex), colour = "black", fill = "white") +
    ggplot2::geom_point(data = dplyr::filter(data, significant), ggplot2::aes(x = module, y = model_sex, colour = estimate, size = abs(estimate))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_size(guide = "none", limits = c(0, 0.5)) +
    ggplot2::scale_color_gradient2(limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5), low = "#2166AC", high = "#B2182B", name = "Correlation", guide = ggplot2::guide_colorbar(ticks = FALSE)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_grid(rows = dplyr::vars(age_group), cols = dplyr::vars(cluster_label), scales = "free") +
    sagethemes::theme_sage() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 8),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
      panel.background = ggplot2::element_blank(),
      plot.title.position = "plot",
      panel.grid = ggplot2::element_blank()
    )
}
