

library(pkgutils)


pkg <- "pkgutils"


################################################################################


must_be <- function(got, wanted) if (!identical(got, wanted))
  stop("'got' not identical to 'wanted'")

must_fail <- function(expr) if (tryCatch({
      expr
      TRUE
    }, error = function(e) FALSE))
  stop("evaluating 'expr' did not fail")


################################################################################
#
# Use the package's own functionality to see what's there
#

must_be(length(x <- pkg_files(pkg, "scripts")), 2L)
stopifnot(!check_R_code(x))

must_be(length(pkg_files(pkg, "highlighting")), 2L)

must_be(length(pkg_files(pkg, "auxiliary")), 2L)


################################################################################
#
# Check the datasets
#

stopifnot(nrow(data(package = pkg)$results) == 1L)


################################################################################
#
# Tests for case()
#

must_be(case(0, "a", "b"), "a")
must_be(case(1, "a", "b"), "b")
must_be(case(10, "a", "b"), "b")
must_fail(case(NA_real_, "a", "b"))
must_fail(case(-1, "a", "b"))
must_be(case("a", a = "a", b = "b"), "a")
must_be(case("b", a = "a", b = "b"), "b")
must_fail(case("c", a = "a", b = "b"))
must_fail(case(NA_character_, a = "a", b = "b"))


################################################################################
#
# Tests for must(), L() and LL()
#

x <- 3
y <- 9:10
z <- 'a'
must_be(c("x", "z"), LL(x, z))
must_fail(LL(x, y))
must_fail(LL(x, y, .wanted = 2L))
must_fail(LL(y, z, .wanted = 2L))
must_be("y", LL(y, .wanted = 2L))
must_fail(must(warning("abc")))




