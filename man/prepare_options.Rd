\name{prepare_options}
\alias{prepare_options}
\alias{prepare_options.NULL}
\alias{prepare_options.character}
\title{Prepare command-line options}
\usage{
  prepare_options(x)

  \method{prepare_options}{NULL} (x)

  \method{prepare_options}{character} (x)
}
\arguments{
  \item{x}{Character vector or \code{NULL}.}
}
\value{
  Character vector.
}
\description{
  Simply routine to add leading dashes to strings for use
  as command-line options. Works with options arguments,
  but only if these are separated from the option by
  \sQuote{=}.
}
\keyword{internal}

