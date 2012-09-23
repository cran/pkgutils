\name{run_pkgutils_ruby}
\alias{run_pkgutils_ruby}
\alias{run_pkgutils_ruby.character}
\title{Run a Ruby script provided by the package}
\usage{
  run_pkgutils_ruby(x, ...)

  \method{run_pkgutils_ruby}{character} (x, script, ignore,
    ...)
}
\arguments{
  \item{script}{Character scalar indicating the name of the
  script to use (without directory name).}

  \item{...}{Optional arguments passed to
  \code{\link{run_ruby}}.}

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
  Result of a call to \code{\link{run_ruby}}.
}
\description{
  Run Ruby with one of the Ruby scripts that come with the
  \pkg{pkgutils} package, and a selection of input files.
}
\keyword{internal}

