\name{L}
\alias{L}
\title{Assert a length}
\usage{
  L(x, .wanted = 1L,
    .msg = "need object '\%s' of length \%i",
    .domain = NULL)
}
\arguments{
  \item{x}{\R object to test.}

  \item{.wanted}{Integer scalar giving the desired length.
  Note that this can \strong{not} be a scalar with
  \sQuote{double} as \code{storage.mode}.}

  \item{.msg}{Error message passed to \code{sprintf} with
  the name of \code{x} and the value of \code{wanted} as
  the two additional arguments.}

  \item{.domain}{Passed to \code{stop} from the \pkg{base}
  package as argument \sQuote{domain}.}
}
\value{
  If successful, \code{x}, but an error message is raised
  if \code{length(x)} is not identical to \code{wanted}.
}
\description{
  Raise an error if a given \R object does not have the
  specified length. This is mainly used to easily generate
  meaningful error messages related to function arguments.
}
\examples{
(x <- L(letters, 26L))
stopifnot(identical(x, letters))
(x <- try(L(letters, 25L), silent = TRUE))
stopifnot(inherits(x, "try-error"))
}
\seealso{
  base::stop

  Other auxiliary-functions: \code{\link{LL}},
  \code{\link{case}},   \code{\link{flatten}},
  \code{\link{listing}},
        \code{\link{logfile}},   \code{\link{map_files}},
  \code{\link{must}},
  \code{\link{run_ruby}},     \code{\link{sections}},
    }
\keyword{utilities}

