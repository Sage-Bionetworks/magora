#' Boxplot for phenotype data
#'
#' @param data Input data (\code{\link{phenotypes}} filtered by phenotype, mouse line, and tissue, with mouse lines expanded via \code{\link{expand_mouse_line_factor_from_selection}}).
#' @inheritParams magora_volcano_plot
#' @param use_theme_sage Whether to use \code{\link[sagethemes]{theme_sage}}. Defaults to TRUE.
#'
#' @return A ggplot2 object.
#' @export
magora_boxplot <- function(data, type = "ggplot2", facet = TRUE, use_theme_sage = TRUE) {

  # Check arguments
  if (!type %in% c("ggplot2", "plotly")) {
    stop("`type` should be one of 'ggplot2' or 'plotly'", call. = FALSE)
  }

  # Generate annotation for mouse lines (facets) that won't have any data
  measured_annotation <- data %>%
    dplyr::count(.data$mouse_line, .drop = FALSE) %>%
    dplyr::filter(.data$n == 0) %>%
    dplyr::mutate(label = "This phenotype cannot be\nmeasured in this mouse line.")

  # If data only contains one sex, generate fake data and set alpha and color so that boxplot legend/dodging are correct
  data <- data %>%
    dplyr::mutate(
      alpha = 0.8,
      color = "black"
    )
  if (length(as.character(unique(data[["sex"]]))) != length(levels(data[["sex"]]))) {
    missing_sex <- levels(data[["sex"]])[levels(data[["sex"]]) != as.character(unique(data[["sex"]]))]
    data <- data %>%
      dplyr::bind_rows(
        data %>%
          dplyr::mutate(
            sex = missing_sex,
            alpha = 0,
            color = NA_character_
          )
      ) %>%
      dplyr::mutate(
        sex = forcats::fct_expand(.data$sex, levels(data[["sex"]])),
        sex = forcats::fct_relevel(.data$sex, levels(data[["sex"]]))
      )
  }

  p <- ggplot2::ggplot(data)

  if (facet) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$mouse_line), ncol = 2, drop = FALSE)
  }

  p <- p +
    ggplot2::geom_boxplot(ggplot2::aes(x = .data$age, y = .data$value, fill = paste("Sex:", .data$sex), color = .data$color, alpha = .data$alpha), position = ggplot2::position_dodge2(preserve = "single"), outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(x = .data$age, y = .data$value, fill = paste("Sex:", .data$sex), alpha = .data$alpha), position = ggplot2::position_jitterdodge(jitter.width = 0.1, seed = 1234)) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_color_identity()

  if (nrow(measured_annotation) > 0) {
    y_range <- ggplot2::layer_scales(p)$y$range$range
    y_mid <- (y_range[[2]] + y_range[[1]]) / 2
    x_mid <- length(levels(data[["age"]])) / 2 + 0.5

    p <- p +
      ggplot2::geom_text(data = measured_annotation, mapping = ggplot2::aes(x = x_mid, y = y_mid, label = .data$label), size = 5, vjust = 0.5, family = ifelse(use_theme_sage, "Lato", ""))
  }

  p <- p +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = "Age", y = unique(data[["phenotype"]])) +
    sagethemes::scale_fill_sage_d()

  if (use_theme_sage) {
    p <- p +
      sagethemes::theme_sage(base_size = 16)
  }

  p <- p +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )

  if (type == "plotly") {
    p <- plotly::ggplotly(p) %>%
      plotly::layout(boxmode = "group")
  }

  p
}
