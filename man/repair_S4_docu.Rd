\name{repair_S4_docu}
\alias{repair_S4_docu}
\alias{repair_S4_docu.character}
\title{Repair S4 documentation in Rd files}
\usage{
  repair_S4_docu(x, ...)

  \method{repair_S4_docu}{character} (x, ...,
    ignore = NULL)
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
  Check examples in Rd files. They should not be present if
  \sQuote{keywords} contain \sQuote{internal} and be
  present otherwise, unless \sQuote{docType} is
  \sQuote{class} or \sQuote{package}.
}
\details{
  This reparation process is currently implemented in a
  Ruby script that comes with the package. It is
  automatically found in the installation directory but
  fails if a suitable version of Ruby, i.e. \eqn{\ge
  1.9.0}, is unavailable. See \code{\link{run_ruby}} for
  further details.
}
\examples{
# See the 'docu.R' script provided with the package, option '--s4methods'.
}
\seealso{
  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

