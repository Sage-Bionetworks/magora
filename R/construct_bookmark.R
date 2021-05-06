construct_bookmark <- function(page, input, session, exclude = NULL) {
  # Bookmark page
  bookmark_page <- list(page = page)

  # Get names of inputs for bookmarking
  inputs <- names(input)
  # Exclude any supplied
  inputs <- setdiff(inputs, exclude)
  inputs <- setdiff(inputs, "bookmark")

  # Prep input values for including in a URL, and collapse multiple using ,
  bookmark_inputs <- purrr::map(inputs, function(x) {
    input[[x]] %>%
      curl::curl_escape() %>%
      paste0(collapse = ",")
  })
  names(bookmark_inputs) <- inputs

  # Combine page and inputs bookmarking
  bookmark <- append(bookmark_page, bookmark_inputs)

  bookmark_names <- names(bookmark)
  bookmark_values <- unname(unlist(bookmark))

  # Create the query string
  url_query <- paste0(bookmark_names, "=", bookmark_values, collapse = "&")

  # Parse the rest of the URL data, then combine with the query string for serving back up
  # Much of this is taken directly from how it's done in Shiny: https://github.com/rstudio/shiny/blob/master/R/shiny.R#L1671
  clientData <- session$clientData

  paste0(
    clientData$url_protocol, "//",
    clientData$url_hostname,
    ifelse(nzchar(clientData$url_port), paste0(":", clientData$url_port), ""),
    clientData$url_pathname,
    "?", url_query
  )
}

# Split string containing , into multiple elements
split_query <- function(query, sep = ",") {
  purrr::map(query, ~ strsplit(.x, sep)[[1]])
}
