\name{flatten}
\alias{flatten}
\alias{flatten.list}
\title{Flatten a list}
\usage{
  flatten(object, ...)

  \method{flatten}{list} (object, ...)
}
\arguments{
  \item{object}{List.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  A list.
}
\description{
  Create a non-nested list.
}
\note{
  The method is based on
  \url{http://stackoverflow.com/questions/8139677/} with
  some slight improvements.
}
\examples{
x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
  e = list(pi))
(y <- flatten(x))
stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
}
\seealso{
  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{listing}},       \code{\link{logfile}},   \code{\link{map_files}},
  \code{\link{must}},
  \code{\link{run_ruby}},     \code{\link{sections}},
    }
\keyword{manip}

