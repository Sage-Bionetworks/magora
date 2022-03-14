#' Pathology page UI function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_pathology_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Pathology"

  shiny::tabPanel(
    title,
    value = title,
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        shiny::includeMarkdown(app_sys("app", "www", "content", "pathology", "content.md")),
        mod_details_modal_ui(ns("pathology")),
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("phenotype"),
            "Phenotype",
            choices = stats::setNames(unique(magora::pathology[["phenotype"]]), unique(magora::pathology[["phenotype_display"]]))
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("mouse_model_group"),
            "Mouse model",
            choices = as.character(levels(magora::pathology[["mouse_model_group"]])),
            multiple = TRUE,
            selected = c("5xFAD", "3xTg-AD")
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("tissue"),
            "Tissue",
            choices = unique(magora::pathology[["tissue"]])
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
        shiny::uiOutput(ns("phenotype_plot_ui"))
      )
    )
  )
}

#' Pathology page server function
#'
#' @noRd
mod_pathology_server <- function(input, output, session) {
  ns <- session$ns

  # Observe any bookmarking to update inputs with ----

  # Initialize reactive value with 1 to flag whether tissue should be updated when phenotype changes
  # Want it to NOT change the first time the bookmark is loaded, but any time after
  pathology_r <- shiny::reactiveVal(1)

  shiny::observe({
    # Additional parsing of query to split by ,
    query <- shiny::parseQueryString(session$clientData$url_search)
    query <- split_query(query)
    if (!is.null(query$page)) {
      if (query$page == "Pathology") {
        # Only update inputs that are also in the query string
        query_inputs <- intersect(names(input), names(query))

        # Iterate over them and update
        purrr::walk(query_inputs, function(x) {
          shinyWidgets::updatePickerInput(session, inputId = x, selected = query[[x]])
        })

        # Change reactive to 0 so that tissue doesn't update - but any time after this it will change back to 1
        pathology_r(0)
      }
    }
  })

  # Change it to 1 any time the phenotype is updated
  # Priority = 1 ensures this is run BEFORE the bookmarking, so if there's a bookmark it changes it back to 0
  shiny::observeEvent(input$phenotype,
    priority = 1,
    pathology_r(1)
  )

  # Set up bookmarking ----
  shiny::observeEvent(input$bookmark, {
    bookmark_query <- construct_bookmark("Pathology", input, session, exclude = c("pathology-details", "plot_click"))
    shiny:::showBookmarkUrlModal(bookmark_query)
  })

  # Update tissue options available based on phenotype selected -----
  shiny::observeEvent(input$phenotype, {
    shiny::req(pathology_r() == 1) # Only updating the tissue when the reactive flag says to
    available_tissue <- magora::phenotype_tissue[[input$phenotype]]

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

  filtered_pathology <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_model_group), message = "Please select one or more mouse lines.")
    )

    magora::pathology %>%
      dplyr::filter(
        .data$phenotype %in% input$phenotype,
        .data$mouse_model_group %in% input$mouse_model_group,
        .data$tissue %in% input$tissue
      )
  })

  mouse_models <- shiny::reactive({
    unlist(magora::pathology_mouse_models[input$mouse_model_group])
  })

  # Generate plot ----

  phenotype_plot <- shiny::reactive({
    shiny::req(input$tissue %in% magora::phenotype_tissue[[input$phenotype]])

    shiny::validate(
      shiny::need(nrow(filtered_pathology()) > 0, message = "There is no data for the selected combination.")
    )

    filtered_pathology() %>%
      expand_mouse_model_factor_from_selection(mouse_models()) %>%
      magora_boxplot(use_theme_sage = TRUE)
  })

  output$phenotype_plot <- shiny::renderPlot(phenotype_plot(), res = 96)

  phenotype_plot_dims <- shiny::reactive({
    list(
      nrow = ceiling(length(mouse_models()) / 2),
      ncol = ifelse(length(mouse_models()) == 1, 1, 2)
    )
  })

  output$phenotype_plot_ui <- shiny::renderUI({

    # Validating mouse line input twice, otherwise there's a quartz error in computing the plot height below
    shiny::validate(
      shiny::need(!is.null(input$mouse_model_group), message = "Please select one or more mouse lines.")
    )

    shinycssloaders::withSpinner(shiny::plotOutput(ns("phenotype_plot"),
      height = paste0(phenotype_plot_dims()[["nrow"]] * 400, "px"),
      width = ifelse(phenotype_plot_dims()[["ncol"]] == 1, "60%", "100%")
      # Disable interactive plot for now
      # click = ns("plot_click")
    ),
    color = "#D3DCEF"
    )
  })

  # Modal ----

  drilldown_pathology <- shiny::reactive({
    shiny::req(input$plot_click)
    panel_filter <- glue::glue('{input$plot_click$mapping$panelvar1} == "{input$plot_click$panelvar1}"')
    filtered_pathology() %>%
      dplyr::filter(eval(rlang::parse_expr(panel_filter)))
  })

  drilldown_title <- shiny::reactive({
    glue::glue("Phenotype: {input$phenotype}, Mouse line: {input$plot_click$panelvar1}, Tissue: {input$tissue}")
  })

  output$drilldown_pathology <- plotly::renderPlotly({
    drilldown_pathology() %>%
      expand_mouse_model_factor_from_selection(input$plot_click$panelvar1) %>%
      magora_boxplot(type = "plotly", facet = FALSE, save_name = drilldown_title(), use_theme_sage = TRUE)
  })

  shiny::observeEvent(input$plot_click, {
    shiny::showModal(
      shiny::modalDialog(
        title = drilldown_title(),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shinycssloaders::withSpinner(plotly::plotlyOutput(
          height = "600px",
          ns("drilldown_pathology")
        ),
        color = "#D3DCEF"
        )
      )
    )
  })

  # Save output ----

  phenotype_data_download <- shiny::reactive({
    # Select and rename columns
    data_cols <- filtered_pathology() %>%
      dplyr::select(
        `Mouse Model` = .data$mouse_model,
        Tissue = .data$tissue,
        Sex = .data$sex,
        Age = .data$age,
        Phenotype = .data$phenotype,
        Units = .data$units,
        Value = .data$value
      )

    # Arrange by column values (from left to right)
    data_cols %>%
      dplyr::arrange(!!!rlang::syms(colnames(data_cols)))
  })

  save_name <- shiny::reactive({
    download_name("phenotype", input$phenotype, input$mouse_model_group, input$tissue)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = phenotype_data_download,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plotId = ns("phenotype_plot"),
    data = phenotype_data_download,
    save_name = save_name
  )

  # Details modal ----

  shiny::callModule(mod_details_modal_server, "pathology")
}
