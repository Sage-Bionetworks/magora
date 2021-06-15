#' Volcano plot for gene expression data
#'
#' @param data Gene expression data from \code{\link{gene_expressions}}, optionally filtered.
#' @param data_labels Labels for expression data from \code{\link{gene_expressions}}, optionally filtered. Only required if \code{type} is "ggplot2".
#' @param type Type of plot - one of "ggplot2" or "plotly". Defaults to "ggplot2".
#' @param facet Whether to facet the data by \code{sex} and \code{age}. Defaults to TRUE.
#' @param save_name A name that will be used for saving the plot. Only required / used when \code{type} is "plotly".
#' @param sample_frac The fraction of genes that are "not significant" that will be shown in the plot. Useful when there is a lot of data that slows down rendering. Defaults to 1.
#'
#' @export
magora_volcano_plot <- function(data, data_labels, type = "ggplot2", facet = TRUE, save_name, sample_frac = 1) {

  # Check arguments
  if (!type %in% c("ggplot2", "plotly")) {
    stop("`type` should be one of 'ggplot2' or 'plotly'", call. = FALSE)
  }

  if (type == "ggplot2" & missing(data_labels)) {
    stop("Please supply `data_labels` for labelling plot - required when `type` is 'ggplot2'.", call. = FALSE)
  }

  if (type == "plotly" & missing(save_name)) {
    stop("Please supply `save_name` for saving the plot - required when `type` is 'plotly'.", call. = FALSE)
  }

  # Create data for threshold lines
  # Need to get a bit creative with this, because combining vline and hline legends in ggplot creates a "crossed" legend which is unappealing - to have a legend that is just horizontal, add "dummy" fold change data to the p_value data frame, then set the real fold change line to have the same linetype as that
  fold_change_line <- dplyr::tibble(x = c(-1, 1))
  p_value_line <- dplyr::tibble(y = -log10(0.05), label = "P-value = 0.05") %>%
    dplyr::mutate(label = forcats::fct_expand(label, "Log2 Fold Change = -1, 1")) %>%
    tidyr::complete(label)

  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x = .data$log2foldchange, y = .data$neg_log10_padj, colour = .data$diff_expressed, text = .data$gene), alpha = 0.25) +
    ggplot2::geom_vline(data = fold_change_line, ggplot2::aes(xintercept = x), linetype = "dashed") +
    ggplot2::geom_hline(data = p_value_line, ggplot2::aes(yintercept = y, linetype = label)) +
    ggplot2::scale_colour_manual(values = c("#85070C", "darkgrey", "#164B6E"), name = NULL, guide = ggplot2::guide_legend(override.aes = list(size = 3), order = 1)) +
    ggplot2::scale_linetype_discrete(guide = ggplot2::guide_legend(reverse = TRUE, order = 2), name = NULL) +
    sagethemes::theme_sage() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(legend.position = "top")

  if (facet) {
    p <- p +
      ggplot2::facet_wrap(dplyr::vars(.data$sex, .data$age), nrow = 2, scales = "free", labeller = ggplot2::labeller(age = function(x) {
        glue::glue("{x} Months")
      }))
  }

  if (type == "ggplot2") {
    p +
      ggrepel::geom_text_repel(data = data_labels, ggplot2::aes(x = .data$log2foldchange, y = .data$neg_log10_padj, colour = .data$diff_expressed, label = .data$label), show.legend = FALSE, seed = 1234, max.overlaps = 5, point.size = NA) +
      ggplot2::labs(x = bquote(~ Log[2] ~ "Fold change"), y = bquote(~ -Log[10] ~ "P-Value"))
  } else if (type == "plotly") {
    p <- p +
      ggplot2::labs(x = "Log2 Fold Change", y = "Log 10 P-Value")

    p <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::config(
        toImageButtonOptions = list(format = "png", filename = save_name, height = 600, width = 900, scale = 2),
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "zoom3d", "zoomInGeo", "zoomOutGeo", "zoomInMapbox", "zoomOutMapbox", "autoScale2d", "resetScale2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
      )

    # Remove the "(, 1)" from legends
    for (i in 1:6) {
      p$x$data[[i]]$name <- stringr::str_remove(p$x$data[[i]]$name, "\\(")
      p$x$data[[i]]$name <- stringr::str_remove(p$x$data[[i]]$name, ",1\\)")
    }

    p

  }
}

sample_gene_expressions <- function(data, sample_frac) {
  set.seed(1234)

  not_significant <- data %>%
    dplyr::filter(.data$diff_expressed == "Not Significant") %>%
    dplyr::group_by(.data$sex, .data$age) %>%
    dplyr::sample_frac(size = sample_frac) %>%
    dplyr::ungroup()

  data %>%
    dplyr::filter(.data$diff_expressed != "Not Significant") %>%
    dplyr::bind_rows(not_significant)
}
