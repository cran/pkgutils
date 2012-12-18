\name{LL}
\alias{LL}
\title{Assert lengths}
\usage{
  LL(..., .wanted = 1L,
    .msg = "need object '\%s' of length \%i",
    .domain = NULL)
}
\arguments{
  \item{...}{Any \R objects to test.}

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
  The names of the arguments contained in \code{\dots},
  returned invisibly, if successful. Otherwise an error is
  raised.
}
\description{
  Raise an error if one of several \R objects does not have
  the specified length. This is mainly used to easily
  generate meaningful error messages related to function
  arguments.
}
\examples{
(x <- LL(letters, LETTERS, .wanted = 26L))
stopifnot(x == c("letters", "LETTERS"))
}
\seealso{
  base::stop

  Other auxiliary-functions: \code{\link{L}},
  \code{\link{case}},   \code{\link{flatten}},
  \code{\link{listing}},
        \code{\link{logfile}},   \code{\link{map_files}},
  \code{\link{must}},
  \code{\link{run_ruby}},     \code{\link{sections}},
    }
\keyword{utilities}

