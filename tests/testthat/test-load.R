

test_that("DescToolsGraphics does not load without DescToolsX", {
  expect_error(
    library(DescToolsGraphics),
    NA
  )
})
