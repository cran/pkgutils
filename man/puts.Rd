\name{puts}
\alias{puts}
\alias{puts.Rd}
\alias{puts.class_desc}
\alias{puts.classes_desc}
\alias{puts.pack_desc}
\alias{puts.pack_descs}
\title{Output some R object}
\usage{
  puts(x, file, ...)

  \method{puts}{classes_desc} (x, file, ...)

  \method{puts}{class_desc} (x, file, ...)

  \method{puts}{Rd} (x, file, ...)

  \method{puts}{pack_desc} (x, file, ...)

  \method{puts}{pack_descs} (x, file, ...)
}
\arguments{
  \item{x}{Object of class \sQuote{class_desc},
  \sQuote{classes_desc}, \sQuote{pack_desc},
  \sQuote{pack_descs} or \sQuote{Rd}.}

  \item{file}{Character vector of file names to which the
  data in \code{x} shall be written.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  \code{x}, returned invisibly.
}
\description{
  Generic function for outputting \R objects. Currently
  defined for some of the classes relevant in this package.
}
\seealso{
  base::cat base::write
}
\keyword{internal}

