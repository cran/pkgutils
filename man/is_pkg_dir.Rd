\name{is_pkg_dir}
\alias{is_pkg_dir}
\alias{is_pkg_dir.character}
\title{Check for package directory}
\usage{
  is_pkg_dir(x)

  \method{is_pkg_dir}{character} (x)
}
\arguments{
  \item{x}{Character vector.}
}
\value{
  Logical vector with the same length than \code{x}.
}
\description{
  Check whether a name refers to a package directory.
}
\examples{
(x <- is_pkg_dir(c("foo", "bar", "baz")))
stopifnot(!x)
(x <- is_pkg_dir(find.package(c("tools", "utils"), quiet = TRUE)))
stopifnot(x)
}
\seealso{
  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{repair_S4_docu}},
    \code{\link{run_R_CMD}},
    \code{\link{swap_code}},
  }
\keyword{package}

