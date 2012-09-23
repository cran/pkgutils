\name{check_examples}
\alias{check_examples}
\alias{check_examples.Rd}
\title{Check presence of examples in Rd files}
\usage{
  check_examples(x, ...)

  \method{check_examples}{Rd} (x, file = NULL, ...)
}
\arguments{
  \item{x}{Object of class \sQuote{Rd}.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}

  \item{file}{Optional character scalar indicating the
  filename from which the \sQuote{Rd} object was read.
  Useful when creating messages describing a problem. See
  \code{\link{logfile}}.}
}
\value{
  Logical scalar, \code{TRUE} if examples are either there
  and should be there or are missing and should be missing,
  \code{FALSE} otherwise.  As a side effect, problem
  messages are printed to \code{stderr}. See
  \code{\link{logfile}} for how to send these messages to a
  file.
}
\description{
  Check presence/absence of examples in Rd files. They
  should not be present if the list of \sQuote{keywords}
  contains \sQuote{internal}. Otherwise they should be
  present unless \sQuote{docType} is \sQuote{class} or
  \sQuote{package}.
}
\note{
  This does \strong{not} run the examples, if any, and
  check their results.
}
\examples{
data(xyplot.docu)
(x <- check_examples(xyplot.docu))
stopifnot(x)
}
\seealso{
  Other rd-functions: \code{\link{check_keywords}},
          \code{\link{repair_docu}},   }
\keyword{package}

