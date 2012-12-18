\name{pkg_files}
\alias{pkg_files}
\alias{pkg_files.character}
\title{Find files within package subdirectories}
\usage{
  pkg_files(x, ...)

  \method{pkg_files}{character} (x, what, installed = TRUE,
    ignore = NULL, ...)
}
\arguments{
  \item{x}{Character vector. If \code{installed} is
  \code{TRUE}, the names of installed packages. Otherwise
  names of package directories, which are expanded
  automatically, and/or directly the names of files, which
  may or may not reside within a package. The directory
  must be recognizable by \code{\link{is_pkg_dir}} for the
  expansion to work.}

  \item{what}{Character vector. The subdirectories to
  list.}

  \item{installed}{Logical scalar. If \code{TRUE}, the
  package(s) are searched using \code{find.package}.
  Otherwise \code{pkg} is treated as list of directories
  and/or file names, distinguished using
  \code{\link{is_pkg_dir}}.}

  \item{ignore}{\code{NULL} or a character vector of file
  names (without their directory-name parts) to remove from
  the result. Matching is done case- insensitively. Ignored
  if empty.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  Character vector of file names. Empty if no such files
  are found.
}
\description{
  List files within given subdirectories of a package.
}
\details{
  The character method passes \code{\dots} to
  \code{list.files} from the \pkg{base} package.
}
\examples{
pkg <- find.package(c("tools", "utils"), quiet = TRUE)
(x <- pkg_files(pkg, "R"))
stopifnot(is.character(x), length(x) == 6)
}
\seealso{
  base::list.files base::find.package base::system.file

  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{repair_S4_docu}},
    \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

