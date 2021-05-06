construct_bookmark <- function(page, input, session, exclude = NULL) {
  bookmark_page <- list(page = page)

  inputs <- names(input)
  inputs <- setdiff(inputs, exclude)

  bookmark_inputs <- purrr::map(inputs, function(x) {
    input[[x]] %>%
      curl::curl_escape() %>%
      paste0(collapse = ",")
  })
  names(bookmark_inputs) <- inputs
  bookmark_inputs <- bookmark_inputs[!names(bookmark_inputs) == "bookmark"]

  bookmark <- append(bookmark_page, bookmark_inputs)

  bookmark_names <- names(bookmark)
  bookmark_values <- unname(unlist(bookmark))

  url_query <- paste0(bookmark_names, "=", bookmark_values, collapse = "&")
  clientData <- session$clientData

  paste0(
    clientData$url_protocol, "//", clientData$url_hostname,
    ifelse(nzchar(clientData$url_port), paste0(":", clientData$url_port), ""), clientData$url_pathname,
    "?", url_query
  )
}

split_query <- function(query, sep = ",") {
  purrr::map(query, ~ strsplit(.x, sep)[[1]])
}
