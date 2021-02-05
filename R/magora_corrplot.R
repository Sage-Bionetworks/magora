#' Correlation plot for nanostring versus AMPAD data
#'
#' Creates a correlation plot comparing nanostring data to human genetic data, faceted by age group and cluster.
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


  ggplot2::ggplot() +
    ggplot2::geom_tile(data = data, ggplot2::aes(x = module, y = model_sex), colour = "black", fill = "white") +
    ggplot2::geom_point(data = dplyr::filter(data, significant), ggplot2::aes(x = module, y = model_sex, colour = estimate, size = abs(estimate))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_size(guide = "none", limits = c(0, 0.5)) +
    ggplot2::scale_color_gradient2(limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5), low = "#2166AC", high = "#B2182B", name = "Correlation", guide = ggplot2::guide_colorbar(ticks = FALSE)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_grid(rows = dplyr::vars(age_group), cols = dplyr::vars(cluster_label), scales = "free", space = "free") +
    sagethemes::theme_sage() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 7),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
      panel.background = ggplot2::element_blank(),
      plot.title.position = "plot",
      panel.grid = ggplot2::element_blank()
    )
}
