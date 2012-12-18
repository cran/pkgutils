\name{check_R_code}
\alias{check_R_code}
\alias{check_R_code.character}
\title{Check R code files}
\usage{
  check_R_code(x, ...)

  \method{check_R_code}{character} (x, lwd = 80L,
    indention = 2L, roxygen.space = 1L, comma = TRUE,
    ops = TRUE, parens = TRUE, assign = TRUE,
    modify = FALSE, ignore = NULL, accept.tabs = FALSE,
    what = "R", encoding = "", ...)
}
\arguments{
  \item{lwd}{Numeric scalar. Maximum line width allowed.
  Set this to a reasonably large number to effectively turn
  checking off.}

  \item{indention}{Numeric scalar. Number of spaces used
  for one unit of indention. Set this to 1 to effectively
  turn checking off.}

  \item{roxygen.space}{Numeric scalar. Number of spaces
  expected after \pkg{roxygen2} comments (which start with
  \sQuote{#} followed by a single quote).}

  \item{comma}{Logical scalar indicating whether it should
  be checked that each comma is not preceded by a space but
  followed by a space.}

  \item{ops}{Logical scalar indicating whether it should be
  checked that operators are surrounded by spaces.}

  \item{parens}{Logical scalar indicating whether it should
  be checked that control-flow constructs are not directly
  followed by parentheses, that opening parentheses (and
  brackets) are not followed by a space, and that closing
  parentheses (and brackets) are followed by appropriate
  characters only and are not preceded by a space.}

  \item{assign}{Logical scalar indicating that it should be
  checked that there is no linebreak within named
  function-argument assignments.}

  \item{modify}{Logical scalar indicating whether the
  source code should be modified (non-destructively, of
  course) and input files overwritten (if changes were
  possible). The modifications currently only comprise the
  removal of whitespace from the ends of the lines and
  optionally the replacement of each tabulator by
  \code{indention} numbers of spaces (see also the next
  argument).}

  \item{accept.tabs}{Logical scalar indicating whether
  tabulators are accepted.}

  \item{what}{Character vector naming the subdirectories to
  consider; passed to \code{\link{pkg_files}}}

  \item{encoding}{Character scalar passed as
  \sQuote{.encoding} argument to \code{\link{map_files}}.}

  \item{...}{Optional arguments passed to
  \code{\link{pkg_files}}.}

  \item{x}{Character vector of names of input files, or
  names of \R package directories. The latter will be
  expanded as appropriate. \code{x} is passed to
  \code{\link{pkg_files}} with the \sQuote{installed}
  argument set to \code{FALSE}. See there for further
  details.}

  \item{ignore}{\code{NULL} or character vector with names
  of files to ignore.  Passed to \code{\link{pkg_files}},
  see there for details of how names are matched.}
}
\value{
  Logical vector; see \code{\link{map_files}} for details.
  Here the result is returned invisibly. As a side effect,
  problem messages are printed to \code{stderr}. See
  \code{\link{logfile}} for how to send these messages to a
  file.
}
\description{
  Check certain aspects of the format of \R code files in
  the \sQuote{R} subdirectory of a package (or of any other
  kinds of files).
}
\details{
  This function is intended to ensure a consistent and
  readable \R coding style. Not all problems can be
  checked, however. For instance, \code{+} and \code{-} are
  binary as well as unary operators and should either be
  followed by spaces or not, respectively. Yielding no
  problem messages is thus just a minimum requirement for a
  good coding style. For instance, indentation checking
  does \strong{not} check whether continuation lines have
  the correct number of leading spaces. Rather, checks are
  made line-per-line throughout. In addition to such false
  negatives, \code{check_R_code} falsely complains about
  numbers in exponential notation.
}
\examples{
# Checking the R scripts that come with the package
(scripts <- pkg_files("pkgutils", "scripts"))
if (length(scripts)) {
  result <- check_R_code(scripts) # should not yield any messages
  stopifnot(is.logical(result), names(result) == names(scripts))
} else {
  warning("scripts not found")
}

# See also the 'docu.R' script provided with this package, options
# '--blank', '--jspaces', '--width', '--assignoff', '--commaoff',
# '--opsoff', '--modify', '--good', '--parensoff', '--Rcheck', '--tabs' and
# '--untidy'. Checking can be turned off generally or specifically.
}
\seealso{
  Other package-functions: \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{repair_S4_docu}},
    \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

