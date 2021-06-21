#' Gene Expression Heatmap page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_gene_expression_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Selected genes"

  shiny::tabPanel(
    title,
    value = "GeneExpressionHeatmap",
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", glue::glue("Gene Expression: {title}")),
        shiny::includeMarkdown(app_sys("app", "www", "content", "gene_expression_selected", "content.md")),
        # Same modal as "all" page is used - no issue with duplicated inputs since these are namespaced by page
        mod_details_modal_ui(ns("gene_expression_all")),
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("gene"),
            "Genes",
            choices = sort(unique(magora::gene_expressions[["gene"]])),
            multiple = TRUE,
            selected = "App",
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE, size = 10,
              noneSelectedText = "Enter gene(s) or select from list"
            )
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("mouse_model"),
            "Mouse model",
            choices = names(magora::gene_expressions_tissue),
            selected = names(magora::gene_expressions_tissue)[1:3],
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE, maxOptions = 3)
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("sex"),
            "Sex",
            choices = sort(unique(magora::gene_expressions[["sex"]])),
            selected = sort(unique(magora::gene_expressions[["sex"]])),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        ),
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            ns("age"),
            "Age",
            choices = stats::setNames(sort(unique(magora::gene_expressions[["age"]])), glue::glue('{sort(unique(magora::gene_expressions[["age"]]))} months')),
            selected = sort(unique(magora::gene_expressions[["age"]])),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        )
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 6,
          offset = 6,
          shiny::column(
            width = 4,
            shiny::bookmarkButton(id = ns("bookmark"), label = "Bookmark", style = "width: 100%")
          ),
          shiny::column(
            width = 4,
            mod_download_data_ui(ns("download_data"))
          ),
          shiny::column(
            width = 4,
            mod_download_plot_ui(ns("download_plot"))
          )
        )
      ),
      shiny::column(
        width = 12,
        align = "center",
        shiny::uiOutput(ns("gene_expression_heatmap_ui"))
      )
    )
  )
}

#' Gene Expression page server Function
#'
#' @noRd
mod_gene_expression_heatmap_server <- function(input, output, session, gene_expressions) {
  ns <- session$ns

  # Observe any bookmarking to update inputs with ----
  shiny::observe(priority = 1, {
    query <- shiny::parseQueryString(session$clientData$url_search)
    # Additional parsing of query to split by ,
    query <- split_query(query)
    if (!is.null(query$page)) {
      if (query$page == "GeneExpressionHeatmap") {
        # Only update inputs that are also in the query string
        query_inputs <- intersect(names(input), names(query))

        # Iterate over them and update
        purrr::walk(query_inputs, function(x) {
          shinyWidgets::updatePickerInput(session, inputId = x, selected = query[[x]])
        })
      }
    }
  })

  # Set up bookmarking ----
  shiny::observeEvent(input$bookmark, {
    bookmark_query <- construct_bookmark("GeneExpressionHeatmap", input, session, exclude = "gene_expression_all-details")
    shiny:::showBookmarkUrlModal(bookmark_query)
  })

  # Filter data based on inputs ----

  filtered_gene_expressions <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$gene) & !is.null(input$mouse_model) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more genes, models, sexes, and ages.")
    )

    magora::gene_expressions %>%
      dplyr::filter(
        .data$gene %in% input$gene,
        .data$mouse_model %in% input$mouse_model,
        .data$sex %in% input$sex,
        .data$age %in% input$age
      ) %>%
      regenerate_pvalue()
  })

  # Generate plot ----

  gene_expression_heatmap <- shiny::reactive({
    shiny::validate(
      shiny::need(nrow(filtered_gene_expressions()) > 0, message = "No data available for the selected combination.")
    )

    filtered_gene_expressions() %>%
      dplyr::filter(!is.na(.data$padj)) %>%
      complete_gene_expression_heatmap_data(input$gene, input$mouse_model) %>%
      magora_heatmap()
  })

  output$gene_expression_heatmap <- shiny::renderCachedPlot(
    {
      gene_expression_heatmap()
    },
    cacheKeyExpr = {
      list(
        input$gene,
        input$mouse_model,
        input$sex,
        input$age
      )
    },
    res = 96
  )

  gene_expression_plot_dims <- shiny::reactive({

    list(
      nrow = length(input$gene) * length(unique(magora::gene_expressions[["tissue"]])),
      ncol = length(input$age) * length(input$sex) * length(input$mouse_model)
    )
  })

  output$gene_expression_heatmap_ui <- shiny::renderUI({
    shinycssloaders::withSpinner(shiny::plotOutput(ns("gene_expression_heatmap"),
      height = paste0(200 + gene_expression_plot_dims()[["nrow"]] * 50, "px"),
      width = min(1000, paste0(150 + gene_expression_plot_dims()[["ncol"]] * 50, "px"))
    ),
    color = "#D3DCEF"
    )
  })

  # Save output ----

  gene_expression_data_download <- shiny::reactive({
    filtered_gene_expressions()
  })

  save_name <- shiny::reactive({
    download_name("gene_expression_heatmap", input$gene, input$mouse_model, input$sex, input$age)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = filtered_gene_expressions,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plotId = ns("gene_expression_heatmap"),
    data = filtered_gene_expressions,
    save_name = save_name
  )

  # Details modal ----

  shiny::callModule(mod_details_modal_server, "gene_expression_all")
}
