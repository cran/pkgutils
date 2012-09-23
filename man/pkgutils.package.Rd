\docType{package}
\name{pkgutils.package}
\alias{pkgutils}
\alias{pkgutils.package}
\alias{pkgutils.package-package}
\title{The pkgutils package}
\description{
  Tools useful when creating \R packages, particularly for
  checking the documentation produced by \pkg{roxygen2} and
  for adding support for the documentation of S4 methods,
  but also for checking some aspects of the coding style.
  Further, some functions of use when coding a package and
  others of use in interactive \R sessions. Finally, \R
  syntax highlighting for some text editors.
}
\details{
  The package contains the following non-standard
  subdirectories: \describe{ \item{auxiliary}{Contains
  scripts in other programming languages that are called by
  some functions of this package. Some functionality of the
  \sQuote{docu.R} script needs them.}
  \item{highlighting}{Patterns for highlighting \R syntax
  for use with some text editors. See the files themselves
  for details.} \item{scripts}{Rscript scripts for
  non-interactive use of \R. The one used to create, check
  and modify package documentation is \sQuote{docu.R}.  The
  other scripts have no connection to creating packages but
  might also be useful. See the help messages of these
  scripts (obtained via the \sQuote{--help} switch) for
  details. The scripts are expected to run at least on
  UNIX-like operating systems. Windows users might need to
  install Rtools, see
  \url{http://cran.r-project.org/bin/windows/Rtools/}.  } }
}
\keyword{package}

