
<!-- README.md is generated from README.Rmd. Please edit that file -->

# magora

<!-- badges: start -->

<!-- badges: end -->

The goal of magora is to allow the exploration of Mouse-Agora data (gene
expressions and Alzheimer’s pathology) in a Shiny app.

## Installation

You can install the development version of magora from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Sage-Bionetworks/magora", ref = "main")
```

## Gene Expression Data

In order to run magora locally, the gene expression data in this
repository needs to be converted from an `.rda` file (which is optimally
compressed) into a series of Parquet files (which allow the app to take
advantage of the [`arrow`
package](https://arrow.apache.org/docs/r/index.html) for efficiently
working with large data in memory).

To convert the data, please run the code in
[`data-raw/gene_expressions_parquet.R`](data-raw/gene_expressions_parquet.R):

    Rscript data-raw/gene_expressions_parquet.R

## Usage

Once the gene expression data has been converted, you can run the app
locally via:

``` r
magora::run_app()
```
