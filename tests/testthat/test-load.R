

test_that("DescToolsViz does not load without DescToolsX", {
  expect_error(
    library(DescToolsViz),
    NA
  )
})
