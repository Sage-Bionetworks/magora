# Set up fake data for testing on

set.seed(1234)

df <- expand.grid(
  tissue = c("cortex", "hippocampus"),
  sex = c("Male", "Female"),
  mouse_line = c("BL6", "BL5"),
  age = c(4, 6, 12, 18),
  phenotype = c("Plaque #", "Plaque Size"),
  stringsAsFactors = FALSE
)

df <- dplyr::bind_rows(df, df) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    value = runif(1, 0, 10)
  ) %>%
  dplyr::ungroup()

df <- df %>%
  dplyr::mutate(
    mouse_line_full = ifelse(dplyr::row_number() %in% sample(1:nrow(df), round(nrow(df) / 2)), paste0("5XfAD;", mouse_line), mouse_line),
    value = ifelse(dplyr::row_number() %in% sample(1:nrow(df), 10), NA_real_, value)
  )

test_that("filter_pathology filters according to .phenotype, .mouse_line, and .tissue arguments", {
  output <- filter_pathology(df, "Plaque #", "BL5", "cortex")
  expect_equal(unique(output[["phenotype"]]), "Plaque #")
  expect_equal(unique(output[["mouse_line"]]), "BL5")
  expect_equal(unique(output[["tissue"]]), "cortex")

  output <- filter_pathology(df, "Plaque #", c("BL5", "BL6"), "cortex")
  expect_equal(unique(output[["phenotype"]]), "Plaque #")
  expect_equal(sort(unique(output[["mouse_line"]])), c("BL5", "BL6"))
  expect_equal(unique(output[["tissue"]]), "cortex")

  output <- filter_pathology(df, "Plaque Number", c("BL5", "BL6"), "cortex")
  expect_true(nrow(output) == 0)
})

test_that("filter_pathology removes NA `value` by default, but does not if na.rm = FALSE", {
  output <- filter_pathology(df, c("Plaque #", "Plaque Size"), c("BL5", "BL6"), c("cortex", "hippocampus"))
  na_rows <- output %>%
    dplyr::filter(is.na(value)) %>%
    nrow()
  expect_true(na_rows == 0)

  output <- filter_pathology(df, c("Plaque #", "Plaque Size"), c("BL5", "BL6"), c("cortex", "hippocampus"), na.rm = FALSE)
  na_rows <- output %>%
    dplyr::filter(is.na(value)) %>%
    nrow()
  expect_true(na_rows > 0)
})
