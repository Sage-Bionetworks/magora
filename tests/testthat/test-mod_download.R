test_that("download_name constructs a string that uses the data type, and separates all inputs (including inputs with length > 1) by _", {
  x <- download_name("phenotype", "x", c("y", "z"), "a/b")
  expect_equal(x, "Pathology_x_y_z_ab")

  x <- download_name("gene_expression_volcano", "x", c("y", "z"), "a/b")
  expect_equal(x, "Gene_Expression_Volcano_x_y_z_ab")

  x <- download_name("gene_expression_heatmap", "x", c("y", "z"), "a/b")
  expect_equal(x, "Gene_Expression_Heatmap_x_y_z_ab")
})
