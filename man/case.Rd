\name{case}
\alias{case}
\alias{case.character}
\alias{case.numeric}
\title{Modified switch function}
\usage{
  case(EXPR, ...)

  \method{case}{numeric} (EXPR, ...)

  \method{case}{character} (EXPR, ...)
}
\arguments{
  \item{EXPR}{A character or numeric scalar based on which
  a decision is made.}

  \item{...}{Additional arguments from which to select an
  alternative.}
}
\value{
  Selected value from \code{\dots}.
}
\description{
  An altered \code{switch} statement for stricter flow
  control.
}
\details{
  If \code{EXPR} is a character scalar, the behaviour is
  like the one of \code{switch} with the exception that
  unmatched values within \code{\dots} cause an error. If
  \code{EXPR} is of mode \sQuote{numeric}, the behaviour is
  like \code{switch} but counting starts at 0 and a value
  larger than the number of elements within \code{\dots}
  selects the last element. It is an error if \code{EXPR}
  is negative or if \code{\dots} contains no arguments at
  all.
}
\examples{
# 'numeric' method
(x <- case(0, "a", "b", "c"))
stopifnot(identical(x, "a"))
(x <- case(99, "a", "b", "c"))
stopifnot(identical(x, "c"))

# 'character' method
(x <- case("b", a = "x", b = "y", c = "z"))
stopifnot(identical(x, "y"))
(x <- try(case("d", a = "x", b = "y", c = "z"), silent = TRUE))
stopifnot(inherits(x, "try-error"))
}
\seealso{
  base::switch

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{collect}},
  \code{\link{flatten}},
    \code{\link{listing}},
          \code{\link{must}}
}
\keyword{utilities}

