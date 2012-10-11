

library(pkgutils)


pkg <- "pkgutils"


################################################################################


mustbe <- function(x, y) stopifnot(identical(x, y))

mustfail <- function(expr) {
  stopifnot(inherits(try(expr, silent = TRUE), "try-error"))
}


################################################################################
#
# Use the package's own functionality to see what's there
#

stopifnot(length(x <- pkg_files(pkg, "scripts")) == 3L)
stopifnot(!check_R_code(x))
stopifnot(length(pkg_files(pkg, "highlighting")) == 2L)
stopifnot(length(pkg_files(pkg, "auxiliary")) == 2L)


################################################################################
#
# Check the datasets
#

stopifnot(nrow(data(package = pkg)$results) == 1L)


################################################################################
#
# Tests for case()
#

mustbe(case(0, "a", "b"), "a")
mustbe(case(1, "a", "b"), "b")
mustbe(case(10, "a", "b"), "b")
mustfail(case(NA_real_, "a", "b"))
mustfail(case(-1, "a", "b"))
mustbe(case("a", a = "a", b = "b"), "a")
mustbe(case("b", a = "a", b = "b"), "b")
mustfail(case("c", a = "a", b = "b"))
mustfail(case(NA_character_, a = "a", b = "b"))


################################################################################
#
# Tests for must(), L() and LL()
#

x <- 3
y <- 9:10
z <- 'a'
mustbe(c("x", "z"), LL(x, z))
mustfail(LL(x, y))
mustfail(LL(x, y, .wanted = 2L))
mustfail(LL(y, z, .wanted = 2L))
mustbe("y", LL(y, .wanted = 2L))
mustfail(must(warning("abc")))




