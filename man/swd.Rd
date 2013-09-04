\name{swd}
\alias{listwd}
\alias{listwd.NULL}
\alias{listwd.numeric}
\alias{swd}
\alias{swd.NULL}
\alias{swd.character}
\alias{swd.numeric}
\title{Change or list working directories}
\usage{
  swd(x)

  \method{swd}{NULL} (x)

  \method{swd}{character} (x)

  \method{swd}{numeric} (x)

  listwd(x)

  \method{listwd}{NULL} (x)

  \method{listwd}{numeric} (x)
}
\arguments{
  \item{x}{For \code{swd}, a numeric scalar indicating how
  often to move upwards (i.e., to which parent directory,
  or character vector containing directory names, or
  \code{NULL}. If \code{x} is a negative number, this is
  used to go to one of the working directories used
  earlier, using an internally stored directory list. That
  is, if \code{n} is a numeric scalar, the action of
  \code{swd(n)} is not necessarily the inverse of what
  \code{swd(-n)} is doing.

  If \code{x} is a character vector, its elements passed in
  turn to \code{setwd}.

  For \code{listwd}, \code{x} is an optional numeric scalar
  indicating how many directories (maximally) to show. The
  default is 10.}
}
\value{
  \code{swd} yields \code{NULL}, returned invisibly. As a
  side effect, the name of the resulting working directory
  is printed. This is the only action if \code{x} is
  \code{NULL}. The directory stack registers a new
  directory only via calls to \code{swd} itself, not via
  \code{setwd}.

  For \code{listwd}, a character vector with directory
  names (current one last), returned invisibly. As a side
  effect, the list of at most \code{x} last directories is
  printed together with the numeric indexes that would be
  needed to set them using \code{\link{swd}}, respectively.
}
\description{
  Set the working directory to, e.g., a parent directory of
  the current one, or to a directory visited earlier.
  Alternatively, list the working directories stored by
  using \code{\link{swd}}, or just the current working
  directory if \code{\link{swd}} has not been called yet.
  These is mainly convenience functions for interactive
  sessions.
}
\examples{
## listwd()
(d1 <- getwd())
x <- listwd()
stopifnot(x == d1)
swd(1)
x <- listwd()
stopifnot(x == c(d1, dirname(d1)))
swd(-1)
x <- listwd()
stopifnot(x == c(d1, dirname(d1), d1))

## swd()
(d1 <- getwd())
swd(1) # got to immediate parent directory
stopifnot(d1 != getwd(), dirname(d1) == getwd())
swd(d1) # go back, using a name
stopifnot(d1 == getwd())
swd(1) # go upwards again
stopifnot(d1 != getwd(), dirname(d1) == getwd())
swd(-1) # go back, using the position within the visited directories
stopifnot(d1 == getwd())
swd(-2) # go back, using the position again
stopifnot(d1 == getwd())
}
\seealso{
  base::setwd base::getwd
}
\keyword{environment}

