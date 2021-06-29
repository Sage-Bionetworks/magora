#' Nanostring page UI function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_nanostring_ui <- function(id) {
  ns <- shiny::NS(id)
  title <- "Correlation with Human Disease"

  shiny::tabPanel(
    title,
    value = "Nanostring",
    shiny::div(
      class = "magora-page",
      shiny::div(
        shiny::h3(class = "tab-title", title),
        shiny::includeMarkdown(app_sys("app", "www", "content", "nanostring", "content.md")),
        mod_details_modal_ui(ns("nanostring")),
        shiny::hr()
      ),
      shiny::fluidRow(
        class = "magora-row",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("mouse_model"),
            "Mouse model",
            choices = sort(unique(magora::nanostring_for_plot[["mouse_model"]])),
            selected = sort(unique(magora::nanostring_for_plot[["mouse_model"]])),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("sex"),
            "Sex",
            choices = sort(unique(magora::nanostring_for_plot[["sex"]])),
            selected = sort(unique(magora::nanostring_for_plot[["sex"]])),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            ns("age"),
            "Age",
            choices = levels(magora::nanostring_for_plot[["age_group"]]),
            selected = levels(magora::nanostring_for_plot[["age_group"]]),
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
        shiny::uiOutput(ns("nanostring_plot_ui"))
      )
    )
  )
}

#' Nanostring page server function
#'
#' @noRd
mod_nanostring_server <- function(input, output, session) {
  ns <- session$ns

  # Observe any bookmarking to update inputs with ----
  shiny::observe(priority = 1, {
    query <- shiny::parseQueryString(session$clientData$url_search)
    # Additional parsing of query to split by ,
    query <- split_query(query)
    if (!is.null(query$page)) {
      if (query$page == "Nanostring") {
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
    bookmark_query <- construct_bookmark("Nanostring", input, session, exclude = "nanostring-details")
    shiny:::showBookmarkUrlModal(bookmark_query)
  })

  # Filter data based on inputs ----

  filtered_nanostring <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_model) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more models, sexes, and age groups.")
    )

    magora::nanostring %>%
      dplyr::filter(
        .data$mouse_model %in% input$mouse_model,
        .data$sex %in% input$sex,
        .data$age_group %in% input$age
      )
  })

  filtered_nanostring_for_plot <- shiny::reactive({
    shiny::validate(
      shiny::need(!is.null(input$mouse_model) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more models, sexes, and age groups.")
    )

    magora::nanostring_for_plot %>%
      dplyr::filter(
        .data$mouse_model %in% input$mouse_model,
        .data$sex %in% input$sex,
        .data$age_group %in% input$age
      )
  })

  # Generate plot ----

  nanostring_plot <- shiny::reactive({
    magora_corrplot(filtered_nanostring_for_plot())
  })

  output$nanostring_plot <- shiny::renderPlot(nanostring_plot(), res = 96)

  # Save size will be height = nrow * 5, width = ncol * 5
  nanostring_plot_dims <- shiny::reactive({
    list(
      nrow = length(input$mouse_model) * length(input$sex) * length(input$age) + length(input$age),
      ncol = 2.5
    )
  })

  output$nanostring_plot_ui <- shiny::renderUI({

    # Validating mouse line input twice, otherwise there's a quartz error in computing the plot height below
    shiny::validate(
      shiny::need(!is.null(input$mouse_model) & !is.null(input$sex) & !is.null(input$age), message = "Please select one or more models, sexes, and age groups.")
    )

    min_height <- 150 + 75 + 100 * length(input$age) # top labels, legend, each side facet label
    rows_height <- 150 + 75 + nanostring_plot_dims()[["nrow"]] * 25
    plot_height <- max(min_height, rows_height)

    shinycssloaders::withSpinner(
      shiny::plotOutput(
        ns("nanostring_plot"),
        height = paste0(plot_height, "px"),
        width = "1000px"
      ),
      color = "#D3DCEF"
    )
  })

  # Save output ----

  nanostring_data_download <- shiny::reactive({
    # Select and rename columns
    data_cols <- filtered_nanostring() %>%
      dplyr::select(
        Cluster = .data$cluster,
        Module = .data$module,
        `Mouse Model` = .data$mouse_model,
        Sex = .data$sex,
        `Age Group` = .data$age_group,
        Correlation = .data$correlation,
        `Adjusted P-Value` = .data$p_value
      )

    # Arrange by column values (from left to right)
    data_cols %>%
      dplyr::arrange(!!!rlang::syms(colnames(data_cols)))
  })

  save_name <- shiny::reactive({
    download_name("gene_expression_heatmap", input$gene, input$mouse_model, input$sex, input$age)
  })

  save_name <- shiny::reactive({
    download_name("nanostring", input$mouse_model, input$sex, input$age)
  })

  # Data

  shiny::callModule(mod_download_data_server,
    "download_data",
    data = nanostring_data_download,
    save_name = save_name
  )

  # Plot

  shiny::callModule(mod_download_plot_server,
    "download_plot",
    plotId = ns("nanostring_plot"),
    data = nanostring_data_download,
    save_name = save_name
  )

  # Details modal ----

  shiny::callModule(mod_details_modal_server, "nanostring")
}

## To be copied in the UI
# mod_nanostring_ui("nanostring")

## To be copied in the server
# callModule(mod_nanostring_server, "nanostring")
