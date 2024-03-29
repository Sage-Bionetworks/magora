#' Gene Expression Volcano Plot page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_volcano_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "All genes"

  shiny::tabPanel(
    title,
    value = "GeneExpressionVolcano",
    shiny::div(class = "magora-hr"),
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", glue::glue("Gene Expression: {title}")),
        shiny::includeMarkdown(app_sys("app", "www", "content", "gene_expression_all", "content.md")),
        mod_details_modal_ui(ns("gene_expression_all")),
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("mouse_model"),
            "Mouse model",
            choices = sort(unique(magora::gene_expressions[["mouse_model"]])),
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = sort(unique(magora::gene_expressions[["tissue"]])),
            multiple = FALSE
          )
        ),
        shiny::div(
          class = "top-align-to-input",
          shiny::column(
            width = 2,
            shiny::bookmarkButton(id = ns("bookmark"), label = "Bookmark", class = "magora-button")
          ),
          shiny::column(
            width = 2,
            mod_download_data_ui(ns("download_data"))
          ),
          shiny::column(
            width = 2,
            mod_download_plot_ui(ns("download_plot"))
          ),
        ),
        shiny::column(
          width = 12,
          align = "center",
          shiny::uiOutput(ns("gene_expression_plot_ui"))
        )
      )
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_volcano_server <- function(input, output, session, gene_expressions) {
  ns <- session$ns

  # Observe any bookmarking to update inputs with ----

  # Initialize reactive value with 1 to flag whether tissue should be updated when strain changes
  # Want it to NOT change the first time the bookmark is loaded, but any time after
  gene_expression_volcano_r <- shiny::reactiveVal(1)

  shiny::observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    # Additional parsing of query to split by ,
    query <- split_query(query)
    if (!is.null(query$page)) {
      if (query$page == "GeneExpressionVolcano") {
        # Only update inputs that are also in the query string
        query_inputs <- intersect(names(input), names(query))

        # Iterate over them and update
        purrr::walk(query_inputs, function(x) {
          shinyWidgets::updatePickerInput(session, inputId = x, selected = query[[x]])
        })

        # Change reactive to 0 so that tissue doesn't update - but any time after this it will change back to 1
        gene_expression_volcano_r(0)
      }
    }
  })

  # Change it to 1 any time the strain is updated
  # Priority = 1 ensures this is run BEFORE the bookmarking, so if there's a bookmark it changes it back to 0
  shiny::observeEvent(input$mouse_model,
    priority = 1,
    gene_expression_volcano_r(1)
  )

  # Set up bookmarking ----
  shiny::observeEvent(input$bookmark, {
    bookmark_query <- construct_bookmark("GeneExpressionVolcano", input, session, exclude = c("gene_expression_all-details", "plot_click"))
    shiny:::showBookmarkUrlModal(bookmark_query)
  })

  # Update tissue options available based on strain selected -----

  shiny::observeEvent(input$mouse_model, {
    shiny::req(gene_expression_volcano_r() == 1) # Only updating the tissue when the reactive flag says to
    available_tissue <- sort(magora::gene_expressions_tissue[[input$mouse_model]])

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
    magora::gene_expressions %>%
      dplyr::filter(
        .data$mouse_model == input$mouse_model,
        .data$tissue == input$tissue
      ) %>%
      regenerate_pvalue()
  })

  filtered_gene_expressions_labels <- shiny::reactive({
    magora::gene_expressions_labels %>%
      dplyr::filter(
        .data$mouse_model == input$mouse_model,
        .data$tissue == input$tissue
      )
  })

  # Generate plot ----

  gene_expression_plot <- shiny::reactive({
    filtered_gene_expressions() %>%
      sample_gene_expressions(0.25) %>%
      magora_volcano_plot(data_labels = filtered_gene_expressions_labels(), type = "ggplot2", facet = TRUE, use_theme_sage = TRUE)
  })

  output$gene_expression_plot <- shiny::renderCachedPlot(gene_expression_plot(),
    cacheKeyExpr = {
      list(
        input$mouse_model,
        input$tissue
      )
    },
    res = 96
  )

  gene_expression_plot_dims <- shiny::reactive({
    list(
      nrow = length(unique(filtered_gene_expressions()[["sex"]])),
      ncol = length(unique(filtered_gene_expressions()[["age"]]))
    )
  })

  output$gene_expression_plot_ui <- shiny::renderUI({
    shinycssloaders::withSpinner(shiny::plotOutput(ns("gene_expression_plot"),
      height = paste0(gene_expression_plot_dims()[["nrow"]] * 400, "px"),
      width = ifelse(gene_expression_plot_dims()[["ncol"]] == 1, "60%", "100%"),
      click = ns("plot_click")
    ),
    color = "#D3DCEF"
    )
  })

  drilldown_gene_expressions <- shiny::reactive({
    shiny::req(input$plot_click)
    panel_filter <- glue::glue('{input$plot_click$mapping$panelvar1} == "{input$plot_click$panelvar1}" & {input$plot_click$mapping$panelvar2} == "{input$plot_click$panelvar2}"')
    filtered_gene_expressions() %>%
      dplyr::filter(eval(rlang::parse_expr(panel_filter))) %>%
      dplyr::filter(!is.na(.data$diff_expressed))
  })

  drilldown_gene_expressions_title <- shiny::reactive({
    glue::glue("Mouse model: {input$mouse_model}, Tissue: {input$tissue}, Sex: {input$plot_click$panelvar2}, Age: {input$plot_click$panelvar1} Months")
  })

  output$drilldown_gene_expressions <- plotly::renderPlotly({
    drilldown_gene_expressions() %>%
      magora_volcano_plot(type = "plotly", facet = FALSE, save_name = drilldown_gene_expressions_title(), use_theme_sage = TRUE)
  })

  shiny::observeEvent(input$plot_click, {
    shiny::showModal(
      shiny::modalDialog(
        title = drilldown_gene_expressions_title(),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shinycssloaders::withSpinner(plotly::plotlyOutput(
          height = "600px",
          ns("drilldown_gene_expressions")
        ),
        color = "#D3DCEF"
        )
      )
    )
  })

  # Save output ----

  gene_expression_data_download <- shiny::reactive({
    # Select and rename columns
    data_cols <- filtered_gene_expressions() %>%
      dplyr::select(
        `Mouse Model` = .data$mouse_model,
        Tissue = .data$tissue,
        Sex = .data$sex,
        Age = .data$age,
        Gene = .data$gene,
        `Differentially Expressed` = .data$diff_expressed,
        `Log2 Fold Change` = .data$log2foldchange,
        `Adjusted P-Value` = .data$padj
      )

    # Arrange by column values (from left to right)
    data_cols %>%
      dplyr::arrange(!!!rlang::syms(colnames(data_cols)))
  })

  save_name <- shiny::reactive({
    download_name("gene_expression_volcano", input$mouse_model, input$tissue)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = gene_expression_data_download,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plotId = ns("gene_expression_plot"),
    data = gene_expression_data_download,
    save_name = save_name
  )

  # Details modal ----

  shiny::callModule(mod_details_modal_server, "gene_expression_all")
}
