library(testthat)
context("Testing the package functions of the pkgutils package")


################################################################################


## pack_desc
## UNTESTED

## pkg_files
test_that("package files can be found", {
  pkg <- "pkgutils"
  x <- pkg_files(pkg, "scripts")
  expect_equal(length(x), 3L)
  x <- pkg_files(pkg, "highlighting")
  expect_equal(length(x), 2L)
  x <- pkg_files(pkg, "auxiliary")
  expect_equal(length(x), 2L)
})

## is_pkg_dir
## UNTESTED

## run_R_CMD
## UNTESTED

## copy_pkg_files
## UNTESTED

## delete_o_files
## UNTESTED

## swap_code
## UNTESTED

## check_R_code
test_that("R code of the package scripts passes the style checks", {
  expect_false(any(check_R_code(pkg_files("pkgutils", "scripts"))))
})

## check_Sweave_start
## UNTESTED

## logfile
## UNTESTED

