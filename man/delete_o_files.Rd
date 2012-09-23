\name{delete_o_files}
\alias{delete_o_files}
\alias{delete_o_files.character}
\title{Delete object files}
\usage{
  delete_o_files(x, ...)

  \method{delete_o_files}{character} (x, ext = "o",
    ignore = NULL, ...)
}
\arguments{
  \item{ext}{Character vector with file extensions to
  consider. Need not contain the leading dot. Content of
  \code{.Platform$dynlib.ext} is added automatically}

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
  Logical vector indicating whether deletion succeeded. See
  \code{file.remove} from the \pkg{base} package.
}
\description{
  If present, delete object files in the \sQuote{src}
  subdirectory of a package remaining from previous
  compilation attempts.
}
\examples{
# See the 'docu.R' script provided with this package, option '--zapoff'.
}
\seealso{
  base::file.remove

  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{repair_S4_docu}},
    \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

