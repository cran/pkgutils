\name{logfile}
\alias{logfile}
\alias{logfile.NULL}
\alias{logfile.character}
\title{Get or set logfile}
\usage{
  logfile(x)

  \method{logfile}{NULL} (x)

  \method{logfile}{character} (x)
}
\arguments{
  \item{x}{Character scalar for setting the logfile, or
  \code{NULL} for getting the current value.}
}
\value{
  Character scalar with the name of the current logfile.
  Use an empty string to turn logging off.
}
\description{
  Get or set the name of the logfile used by
  \pkg{pkgutils}.
}
\details{
  Functions such as \code{\link{check_keywords}} print
  detected problems, if any, using \code{message}. These
  character vectors can also be safed by appending to a
  logfile.
}
\examples{
old <- logfile()
new <- tempfile()
logfile(new)
stopifnot(new == logfile())
logfile(old)
stopifnot(old == logfile())
}
\seealso{
  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{flatten}},   \code{\link{listing}},
          \code{\link{map_files}},
  \code{\link{must}},
  \code{\link{run_ruby}},     }
\keyword{IO}

