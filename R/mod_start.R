#' Start page UI Function
#'
mod_start_ui <- function() {
  shiny::tabPanel(
    "Start",
    shiny::div(
      class = "magora-page",
      shiny::div(
        class = "start-banner",
        shiny::div(class = "start-banner-img"),
        shiny::div(class = "start-title", shiny::h1("MODEL-AD Mouse Explorer")),
      ),
      shiny::p(class = "intro", "Explore gene expression and pathology data from next generation mouse models of Alzheimer's disease developed by the MODEL-AD consortium."),
      shiny::p(class = "intro", "MODEL-AD comprises two research centers with complementary approaches to generating new mouse models that more faithfully recapitulate features of Alzheimer's disease in humans."),
      shiny::p(class = "intro", shiny::a("Learn more about MODEL-AD", href = "https://www.model-ad.org/", target = "_blank")),
      shiny::div(class = "start-divider"),
      shiny::h3("Characterization of MODEL-AD mice"),
      shiny::p("New mouse models are phenotyped across multiple neuropathology, genomics, and behavioral levels. New data and analyses are regularly released and will be reflected by updates to the explorers on this site. Currently available models and data are depicted in this table."),
      shiny::h3("Additional resources and data"),
      shiny::p("All MODEL-AD mouse models and data are cataloged in the AD Knowledge Portal."),
      shiny::p(
        shiny::a("View all data", href = "https://adknowledgeportal.synapse.org/Explore/Programs/DetailsPage?Program=MODEL-AD", target = "_blank")
      ),
      shiny::p(
        shiny::a("View mouse models", href = "https://adknowledgeportal.synapse.org/Explore/Experimental%20Tools?QueryWrapper0=%7B%22sql%22%3A%22select%20*%20from%20syn22219805%22%2C%22limit%22%3A25%2C%22offset%22%3A0%2C%22selectedFacets%22%3A%5B%7B%22concreteType%22%3A%22org.sagebionetworks.repo.model.table.FacetColumnValuesRequest%22%2C%22columnName%22%3A%22program%22%2C%22facetValues%22%3A%5B%22MODEL-AD%22%5D%7D%5D%7D", target = "_blank")
      )
    )
  )
}
