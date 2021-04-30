test_that("magora_volcano_plot errors if type is not ggplot2 or plotly", {
  expect_error(magora_volcano_plot(type = 'ggplot'), regexp = "one of 'ggplot2' or 'plotly'")
})

test_that("magora_volcano_plot errors if type is ggplot2 and data_labels is not given", {
  expect_error(magora_volcano_plot(gene_expressions), regexp = 'supply `data_labels`')
})


test_that("magora_volcano_plot errors if type is plotly and save_name is not given", {
  expect_error(magora_volcano_plot(gene_expressions, type = "plotly"), regexp = 'supply `save_name`')
})
