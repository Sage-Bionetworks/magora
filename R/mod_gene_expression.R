#' Gene Expression page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Gene Expression"

  shiny::tabPanel(
    title,
    shiny::div(
      shiny::h3(class = "tab-title", title),
      shiny::tags$p(class = "tab-description", shinipsum::random_text(nwords = 15)),
      shiny::hr()
    ),
    shiny::fluidRow(
      class = "magora-page",
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("gene"),
            "Gene",
            choices = magora::gene_expression_genes,
            multiple = FALSE,
            selected = "0610007P14Rik",
            options = shinyWidgets::pickerOptions(size = 10, liveSearch = TRUE)
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("mouse_line"),
            "Mouse lines",
            choices = magora::gene_expression_mouse_lines,
            multiple = TRUE,
            selected = c("5XFAD", "C57BL6J")
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = magora::gene_expression_tissues
          )
        ),
        shiny::column(
          width = 3,
          mod_download_data_ui(ns("download_data")),
          mod_download_plot_ui(ns("download_plot"))
        )
      ),
      shiny::column(
        width = 12,
        shiny::uiOutput(ns("gene_expression_plot_ui"))
      )
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_server <- function(input, output, session, gene_expressions) {
  ns <- session$ns

  # Update tissue options based on gene expression selected ----

  shiny::observeEvent(input$mouse_line, {
    available_tissue <- unique(unlist(magora::gene_expressions_mouse_line_tissues[input$mouse_line]))

    # If the tissue previously selected is still available, keep it selected
    selected_tissue <- ifelse(input$tissue %in% available_tissue, input$tissue, available_tissue[[1]])

    shinyWidgets::updatePickerInput(
      session = session,
      "tissue",
      choices = available_tissue,
      selected = selected_tissue
    )
  })

  # Filter data based on inputs ----

  filtered_gene_expressions <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    gene_expressions %>%
      # Cannot use .data pronoun because it is meaningful (for something else) in Arrow
      dplyr::filter(
        partition == tolower(stringr::str_sub(input$gene, 1, 1)),
        gene == input$gene,
        tissue == input$tissue
      ) %>%
      dplyr::collect() %>%
      dplyr::filter(mouse_line %in% input$mouse_line) # Arrow seems to have issue with %in%, so collect, then do the last filter
  })

  # Generate plot ----

  gene_expression_plot <- shiny::reactive({
    shiny::validate(
      shiny::need(nrow(filtered_gene_expressions()) > 0, message = "There is no data for the selected combination.")
    )

    filtered_gene_expressions() %>%
      expand_mouse_line_factor_from_selection(input$mouse_line) %>%
      magora_boxplot(plot_type = "gene expression")
  })

  output$gene_expression_plot <- shiny::renderPlot(gene_expression_plot())

  gene_expression_plot_dims <- reactive({
    list(
      nrow = ceiling(length(input$mouse_line) / 2),
      ncol = ifelse(length(input$mouse_line) == 1, 1, 2)
    )
  })

  output$gene_expression_plot_ui <- shiny::renderUI({

    # Validating mouse line input twice, otherwise there's a quartz error in computing the plot height below
    shiny::validate(
      shiny::need(!is.null(input$mouse_line), message = "Please select one or more mouse lines.")
    )

    shinycssloaders::withSpinner(shiny::plotOutput(ns("gene_expression_plot"),
      height = paste0(gene_expression_plot_dims()[["nrow"]] * 400, "px"),
    ),
    color = "#D3DCEF"
    )
  })

  # Save output ----

  save_name <- reactive({
    download_name("gene_expression", input$gene, input$mouse_line, input$tissue)
  })

  # Data

  callModule(mod_download_data_server,
    "download_data",
    data = filtered_gene_expressions,
    save_name = save_name
  )

  # Plot

  callModule(mod_download_plot_server,
    "download_plot",
    plot = gene_expression_plot,
    data = filtered_gene_expressions,
    save_name = save_name,
    plot_dims = gene_expression_plot_dims
  )
}
