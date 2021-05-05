construct_bookmark <- function(page, input, session) {
  bookmark_page <- list(page = "Pathology")

  bookmark_inputs <- purrr::map(names(input), function(x) {
    input[[x]] %>%
      curl::curl_escape() %>%
      paste0(collapse = ",")
  })
  names(bookmark_inputs) <- names(input)
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
