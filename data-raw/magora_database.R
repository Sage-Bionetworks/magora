library(RSQLite)
library(dplyr)

magora_db <- dbConnect(SQLite(), here::here("data-raw", "magora.sqlite"))
# dbWriteTable(magora_db, "gene_expressions", magora::gene_expressions)

# Sample querying
dbGetQuery(magora_db, "SELECT * FROM gene_expressions LIMIT 5")

# Benchmark just the filtering
filter_db <- function() {
  dbGetQuery(magora_db, "SELECT * FROM gene_expressions WHERE strain == '5xFAD' AND tissue == 'Hemibrain'")

  return(NULL) # Returning NULL because the results are not quite identical - filter_db returns a data.frame, filter_rda a tibble and don't want to waste benchmarking time on the conversion
}

filter_rda <- function() {
  dplyr::filter(magora::gene_expressions, strain == "5xFAD", tissue == "Hemibrain")

  return(NULL)
}

bench::mark(
  filter_db(),
  filter_rda(),
  min_iterations = 10
)

# Results:
# expression       min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# <bch:expr>   <bch:t> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
# 1 filter_db()  580.3ms 582.1ms      1.72    45.9MB     1.15     6     4      3.49s
# 2 filter_rda()  27.1ms  37.5ms     28.1    506.1MB    10.5      8     3   285.05ms
# # … with 4 more variables: result <list>, memory <list>, time <list>, gc <list>

# filter_rda() doesn't take into account just how long it takes to load the package (startup will only happen once, but it's still significant):
bench::mark(devtools::load_all())

# expression             min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
# <bch:expr>           <bch> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>
# 1 devtools::load_all() 7.06s  7.06s     0.142     145MB    0.142     1     1
# # … with 5 more variables: total_time <bch:tm>, result <list>, memory <list>,
# #   time <list>, gc <list>

# This time is due almost entirely to the gene_expressions data - wouldn't take nearly as long to load without it!

# Creating a plot from the database versus from the R file

labels <- magora::gene_expressions_labels %>%
  dplyr::filter(strain == "5xFAD", tissue == "Hemibrain")

dbGetQuery(magora_db, "SELECT * FROM gene_expressions WHERE strain == '5xFAD' AND tissue == 'Hemibrain'") %>%
  magora:::sample_gene_expressions(0.1) %>%
  magora::magora_volcano_plot(data_labels = labels, type = "ggplot2", facet = TRUE)

magora::gene_expressions %>%
  dplyr::filter(strain == "5xFAD", tissue == "Hemibrain") %>%
  magora:::sample_gene_expressions(0.1) %>%
  magora::magora_volcano_plot(data_labels = labels, type = "ggplot2", facet = TRUE)

# Close connection to database
dbDisconnect(magora_db)
