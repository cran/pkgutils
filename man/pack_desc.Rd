\name{pack_desc}
\alias{is_pkg_dir}
\alias{is_pkg_dir.character}
\alias{pack_desc}
\alias{pack_desc.character}
\alias{pkg_files}
\alias{pkg_files.character}
\title{List or modify package (description) files}
\usage{
  pack_desc(pkg, ...)

  \method{pack_desc}{character} (pkg,
    action = c("read", "update", "source"), version = TRUE,
    demo = FALSE, date.format = "\%Y-\%m-\%d",
    envir = globalenv(), ...)

  pkg_files(x, ...)

  \method{pkg_files}{character} (x, what, installed = TRUE,
    ignore = NULL, ...)

  is_pkg_dir(x)

  \method{is_pkg_dir}{character} (x)
}
\arguments{
  \item{pkg}{Name(s) of one to several package directories.
  The package name alone does \strong{not} suffice unless
  the package is a subdirectory of the working directory.}

  \item{action}{Character scalar determining the output
  mode.}

  \item{version}{Logical scalar. Also update the version?
  Only relevant if \code{action} is \sQuote{update}. Note
  that this updating would only affect the last part of the
  version string separated by \sQuote{-}; if this does not
  exist, it is ignored. Updating is \strong{not} done if
  the old date is identical or newer to the new one. Note
  that this comparison only works properly if
  \code{data.format} is correctly specified.}

  \item{demo}{Logical scalar. Do not update or source
  files, just return a description of the result?}

  \item{date.format}{Character scalar. The format used and
  expected for the date of the package.}

  \item{envir}{Environment used when sourceing files. Only
  relevant if \code{action} is set to \sQuote{source}.}

  \item{x}{Character vector. For \code{pkg_files}, if
  \code{installed} is \code{TRUE}, the names of installed
  packages. Otherwise names of package directories, which
  are expanded automatically, and/or directly the names of
  files, which may or may not reside within a package. The
  directory must be recognizable by \code{is_pkg_dir} for
  the expansion to work. For \code{is_pkg_dir}, an
  arbitrary character vector.}

  \item{what}{Character vector. The subdirectories to
  list.}

  \item{installed}{Logical scalar. If \code{TRUE}, the
  package(s) are searched using \code{find.package}.
  Otherwise \code{pkg} is treated as list of directories
  and/or file names, distinguished using
  \code{is_pkg_dir}.}

  \item{ignore}{\code{NULL} or a character vector of file
  names (without their directory-name parts) to remove from
  the result. Matching is done case- insensitively. Ignored
  if empty.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  The value returned by \code{pack_desc} depends on the
  value of \code{action}: \describe{ \item{read}{Object of
  class \sQuote{pack_descs}, basically a nested list with
  \code{pkg} as names. The values are objects of class
  \sQuote{pack_desc}.} \item{update}{One- or two-column
  character matrix with one row per entry in \code{pkg},
  showing the updated date and optionally the version
  string. \code{\dots} is passed to \code{write.dcf} if
  \code{demo} is \code{FALSE}.} \item{source}{This loads
  the \R code files of the package(s) using \code{source}
  from the \pkg{base} package in the correct order, and
  call \code{library} on all the package names given under
  \sQuote{Depends} and \sQuote{Imports}. Thus a list of
  \code{source} results is obtained, returned invisibly.
  \code{\dots} is passed to \code{source} if \code{demo} is
  \code{FALSE}.} }

  \code{pkg_files} yields a character vector of file names
  (empty if no such files are found).

  \code{is_pkg_dir} yields a logical vector with the same
  length than \code{x}.
}
\description{
  \code{pack_desc} reads the \sQuote{DESCRIPTION} file of
  an \R package. Optionally it sets the \sQuote{Date} entry
  to the current date, and if requested increment the
  subversion number of the package version, if any, and
  write the data back to each input file. Alternatively,
  call \code{source} on all \R code files of a package as
  listed in the \sQuote{DESCRIPTION} file. \code{pkg_files}
  lists files within given subdirectories of a package. It
  works on either installed packages or package source
  folders. \code{is_pkg_dir} determines whether names refer
  to such package directories.
}
\examples{
pkg <- find.package(c("tools", "utils"), quiet = TRUE)

# Reading
(x <- pack_desc(pkg, "read")) # should look similar to packageVersion()
stopifnot(is.list(x), names(x) == pkg, inherits(x, "pack_descs"))
stopifnot(sapply(x, is.list), sapply(x, inherits, what = "pack_desc"))

# Updating (in demo mode, of course)
(x <- pack_desc(pkg, "update", demo = TRUE, date.format = "\%Y/\%m/\%d"))
stopifnot(is.character(x), is.matrix(x), rownames(x) == pkg,
  colnames(x) == c("Date", "Version"))
(x <- pack_desc(pkg, "update", demo = TRUE, version = FALSE))
stopifnot(is.character(x), is.matrix(x), rownames(x) == pkg,
  colnames(x) == "Date")

# Source'ing (in demo mode, of course)
(x <- pack_desc(pkg, "source", demo = TRUE))
stopifnot(is.list(x), names(x) == pkg, sapply(x, is.list))
stopifnot(sapply(x, names) == c("depends", "imports", "r.files"))

# See also the 'docu.R' script, options '--format' and '--keep'.

## pkg_files()
pkg <- find.package(c("tools", "utils"), quiet = TRUE)
(x <- pkg_files(pkg, "R"))
stopifnot(is.character(x), length(x) == 6)

## is_pkg_dir()
(x <- is_pkg_dir(c("foo", "bar", "baz")))
stopifnot(!x)
(x <- is_pkg_dir(find.package(c("tools", "utils"), quiet = TRUE)))
stopifnot(x)
}
\seealso{
  base::read.dcf base::write.dcf base::source
  base::list.files base::find.package base::system.file
  utils::packageDescription

  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{logfile}},   \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

