library(testthat)
context("Testing the data of the pkgutils package")


################################################################################

## ?
test_that("the data sets are there", {
  expect_equal(nrow(data(package = "pkgutils")$results), 1L)
})
