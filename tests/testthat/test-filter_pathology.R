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
