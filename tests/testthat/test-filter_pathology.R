test_that("filter_pathology filters according to phenotype, mouse_line, and tissue arguments", {
  output <- filter_pathology(phenotypes, "Plaque #", "BL6", "cortex")
  expect_equal(unique(output[["phenotype"]]), "Plaque #")
  expect_equal(unique(output[["mouse_line"]]), "BL6")
  expect_equal(unique(output[["tissue"]]), "cortex")

  output <- filter_pathology(phenotypes, "Plaque Size",  "BL6", "hippocampus")
  expect_equal(unique(output[["phenotype"]]), "Plaque Size")
  expect_equal(sort(unique(output[["mouse_line"]])), "BL6")
  expect_equal(unique(output[["tissue"]]), "hippocampus")

  output <- filter_pathology(phenotypes, "Plaque Number", "BL6", "cortex")
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
