
################################################################################
#
# This is the testing master file for the package. We use unit testing via the
# 'testthat' package. The proper files with the tests are in the inst/tests
# subdirectory. There is also an inst/testdata subdirectory, which contains the
# files used for testing the IO functions.
#
################################################################################


library(pkgutils)

if (require(testthat) &&
    # 'testhat' does not work under Solaris (Ripley, pers. comm. 14-02-2012);
    # this is a blind-flight attempt to turn it off that seemed to work so far
    !grepl("solaris", R.version$platform, TRUE, TRUE)) {
  test_package("pkgutils")
}
