\name{listwd}
\alias{listwd}
\alias{listwd.NULL}
\alias{listwd.numeric}
\title{List the working directories}
\usage{
  listwd(x)

  \method{listwd}{NULL} (x)

  \method{listwd}{numeric} (x)
}
\arguments{
  \item{x}{Numeric scalar indicating how many directories
  (maximally) to show.}
}
\value{
  Character vector with directory names (current one last),
  returned invisibly. As a side effect, the list of at most
  \code{x} last directories is printed together with the
  numeric indexes that would be needed to set them using
  \code{\link{swd}}, respectively.
}
\description{
  List the working directories stored by using
  \code{\link{swd}}, or just the current working directory
  if \code{\link{swd}} has not been called yet.
}
\examples{
d1 <- getwd()
x <- listwd()
stopifnot(x == d1)
swd(1)
x <- listwd()
stopifnot(x == c(d1, dirname(d1)))
swd(-1)
x <- listwd()
stopifnot(x == c(d1, dirname(d1), d1))
}
\seealso{
  base::setwd base::getwd

  Other directory-functions: \code{\link{swd}},
    }
\keyword{environment}

