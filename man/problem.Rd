\name{problem}
\alias{problem}
\alias{problem.character}
\title{Print message indicating problem}
\usage{
  problem(x, ...)

  \method{problem}{character} (x, infile = NULL,
    line = NULL, ...)
}
\arguments{
  \item{x}{Character vector.}

  \item{infile}{Name of input file. Use \code{NULL} to
  ignore it.}

  \item{line}{Number of input line. Use \code{NULL} to
  ignore it.}
}
\value{
  Character vector.
}
\description{
  Simple utility to print a message indicating a certain
  problem. Optionally also note an input file in which the
  problem occurs.
}
\keyword{internal}

