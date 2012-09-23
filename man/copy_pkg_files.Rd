\name{copy_pkg_files}
\alias{copy_pkg_files}
\alias{copy_pkg_files.character}
\title{Copy packages files}
\usage{
  copy_pkg_files(x, ...)

  \method{copy_pkg_files}{character} (x, what = "scripts",
    to = file.path(Sys.getenv("HOME"), "bin"),
    installed = TRUE, ignore = NULL, overwrite = TRUE, ...)
}
\arguments{
  \item{what}{Character vector with the names of
  subdirectories to copy.}

  \item{to}{Character vector indicating the target
  folder(s) or file(s).}

  \item{overwrite}{Logical scalar passed to
  \code{file.copy} from the \pkg{base} package (in addition
  to \code{from} and \code{to}).}

  \item{...}{Optional arguments passed to the same
  function.}

  \item{x}{Character vector. If \code{installed} is
  \code{TRUE}, the names of installed packages. Otherwise
  names of package directories, which are expanded
  automatically, and/or directly the names of files, which
  may or may not reside within a package. The directory
  must be recognizable by \code{\link{is_pkg_dir}} for the
  expansion to work.}

  \item{installed}{Logical scalar. If \code{TRUE}, the
  package(s) are searched using \code{find.package}.
  Otherwise \code{pkg} is treated as list of directories
  and/or file names, distinguished using
  \code{\link{is_pkg_dir}}.}

  \item{ignore}{\code{NULL} or a character vector of file
  names (without their directory-name parts) to remove from
  the result. Matching is done case- insensitively. Ignored
  if empty.}
}
\value{
  Logical vector. See \code{file.copy} from the \pkg{base}
  package.
}
\description{
  Copy package files after installation. This is mainly
  intended for script files, which should be placed in a
  directory for executables.
}
\examples{
# See the 'docu.R' script provided with this package, options '--target' and
# '--exclude'.
}
\seealso{
  base::file.copy

  Other package-functions: \code{\link{check_R_code}},
    \code{\link{delete_o_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{repair_S4_docu}},
    \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

