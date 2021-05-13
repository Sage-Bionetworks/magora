library(RSQLite)

magora_db <- dbConnect(SQLite(), here::here("data-raw", "magora.sqlite"))
dbWriteTable(magora_db, "gene_expressions", magora::gene_expressions)

# Sample querying
dbGetQuery(magora_db, "SELECT * FROM gene_expressions LIMIT 5")
