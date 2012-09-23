\name{swap_code}
\alias{swap_code}
\alias{swap_code.character}
\title{Preprocess R files}
\usage{
  swap_code(x, ...)

  \method{swap_code}{character} (x, ..., ignore = NULL)
}
\arguments{
  \item{x}{Character vector of names of input files, or
  names of \R package directories. The latter will be
  expanded as appropriate. \code{x} is passed to
  \code{\link{pkg_files}} with the \sQuote{installed}
  argument set to \code{FALSE}. See there for further
  details.}

  \item{...}{Optional arguments, currently passed as
  arguments additional to \sQuote{x} to
  \code{\link{run_ruby}}. See there for details.}

  \item{ignore}{\code{NULL} or character vector with names
  of files to ignore.  Passed to \code{\link{pkg_files}},
  see there for details of how names are matched.}
}
\value{
  Currently the return value of the call to
  \code{\link{run_ruby}}.
}
\description{
  Preprocess \R code files using a simple swapping
  algorithm. Files are modified in-place, hence this is
  potentially dangerous and should best be applied if the
  package directory to document, check and/or install is
  not the one in which coding is done but a copy of it.
}
\details{
  The code preprocessing works simply as follows: Lines are
  split at the first occurence of \sQuote{#||}, if any, the
  parts reversed and joined again with a space character,
  including the separator. Leading whitespace is kept.
  Whitespace around the parts, if any, is removed
  (effectively transformed to a single space). There is
  \strong{no} check done to ensure that the part moved in
  front of the comment character is syntactically valid in
  its context, or correct or useful code. For instance, the
  line \samp{ SEALED <- FALSE #|| SEALED <- TRUE} would be
  modified to \samp{ SEALED <- TRUE #|| SEALED <- FALSE},
  i.e. this could be used to change a package constant
  before conducting any checking.
}
\note{
  This preprocessing is currently implemented in a Ruby
  script that comes with the package. It is automatically
  found in the installation directory but fails if a
  suitable version of Ruby, i.e. \eqn{\ge 1.9.0}, is
  unavailable. See \code{\link{run_ruby}} for details.
}
\examples{
# See the 'docu.R' script provided with the package, option '--preprocess'.
}
\seealso{
  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{repair_S4_docu}},
    \code{\link{run_R_CMD}},
  }
\keyword{package}

