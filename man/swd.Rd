\name{swd}
\alias{swd}
\alias{swd.NULL}
\alias{swd.character}
\alias{swd.numeric}
\title{Change working directory}
\usage{
  swd(x)

  \method{swd}{NULL} (x)

  \method{swd}{character} (x)

  \method{swd}{numeric} (x)
}
\arguments{
  \item{x}{Numeric scalar indicating how often to move
  upwards (i.e., to which parent directory, or character
  vector containing directory names, or \code{NULL}. If
  \code{x} is a negative number, this is used to go to one
  of the working directories used earlier, using an
  internally stored directory list. If \code{x} is a
  character vector, its elements passed in turn to
  \code{setwd}.}
}
\value{
  \code{NULL}, returned invisibly. As a side effect, the
  name of the resulting working directory is printed. This
  is the only action if \code{x} is \code{NULL}.
}
\description{
  Set the working directory to, e.g., a parent directory of
  the current one, or to a directory visited earlier. This
  is mainly a convenience function for interactive
  sessions.
}
\details{
  The directory stack registers a new directory only via
  calls to \code{swd} itself, not via \code{setwd}.
}
\note{
  If \code{n} is a numeric scalar, the action of
  \code{swd(n)} is not necessarily the inverse of what
  \code{swd(-n)} is doing.
}
\examples{
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

  Other directory-functions: \code{\link{listwd}},
  }
\keyword{environment}

