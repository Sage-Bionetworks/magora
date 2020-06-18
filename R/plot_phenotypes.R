plot_phenotypes <- function(.data, .phenotype) {

  ggplot2::ggplot(.data, ggplot2::aes(x = .data$Age, y = .data$value, fill = .data$Sex)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge2(width = 0.5, preserve = "single")) +
    ggplot2::facet_wrap(ggplot2::vars(.data$`Mouse Line`)) +
    ggplot2::labs(x = "Age", y = .phenotype) +
    sagethemes::scale_fill_sage_d() +
    sagethemes::theme_sage()

}
