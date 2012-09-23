\name{rd_quote}
\alias{rd_quote}
\alias{rd_quote.character}
\title{Enclose text in Rd commands}
\usage{
  rd_quote(x, ...)

  \method{rd_quote}{character} (x, text, ...)
}
\arguments{
  \item{x}{Character vector of Rd command names (outermost
  first). It is an error if an entry in \code{x} contains
  special characters from Rd syntax, but empty character
  strings and \code{NA} values are just ignored.}

  \item{text}{Character vector or convertible to such; text
  to be enclosed.  Needs not be appropriately escaped
  beforehand. Must not contain \code{NA}.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  Character vector of the same length than \code{text}.
}
\description{
  Enclose text in Rd commands after escaping it
  appropriately.
}
\seealso{
  tools::toRd
}
\keyword{internal}

