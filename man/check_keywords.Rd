\name{check_keywords}
\alias{check_keywords}
\alias{check_keywords.NULL}
\alias{check_keywords.Rd}
\alias{check_keywords.character}
\alias{check_keywords.logical}
\title{Check keywords in Rd files}
\usage{
  check_keywords(x, ...)

  \method{check_keywords}{NULL} (x, full = FALSE, ...)

  \method{check_keywords}{character} (x, ...)

  \method{check_keywords}{logical} (x, ...)

  \method{check_keywords}{Rd} (x, file = NULL,
    list.only = FALSE, ...)
}
\arguments{
  \item{x}{Object of class \sQuote{Rd}, vector, or
  \code{NULL}. \sQuote{Rd} objects are checked as described
  below. If \code{NULL}, the total list of known keywords
  is returned as character vector, otherwise only those
  contained in \code{x}. If a logical vector, \code{x} is
  used like the \sQuote{full} argument. If a character
  vector, used to select the descriptions of certain
  keywords.}

  \item{file}{Optional character scalar indicating the
  filename from which the \sQuote{Rd} object was read.
  Useful when creating messages describing a problem. See
  \code{\link{logfile}}.}

  \item{full}{Logical scalar indicating whether the full
  content of the \sQuote{KEYWORDS.db} file should be
  returned, not just the list of keywords.}

  \item{list.only}{Logical scalar indicating whether the
  keywords should just be collected, not checked against
  the database.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  Character vector. As a side effect, problem messages are
  printed to \code{stderr}. See \code{\link{logfile}} for
  how to send these messages to a file.
}
\description{
  Check keywords in Rd files for their presence, for their
  occurrence within the list given in the
  \sQuote{KEYWORDS.db} file that comes with \R{}, and for
  fitting to the \sQuote{docType} of the \sQuote{Rd}
  object.
}
\details{
  The \sQuote{Rd} method examines the content of Rd files.
  In addition to checking that the keywords used are listed
  in the \sQuote{KEYWORDS.db} file, it is checked that
  \sQuote{internal} is not combined with other keywords,
  and that \sQuote{docType} entries such as \sQuote{class},
  \sQuote{data} or \sQuote{package} are accompanied by the
  appropriate keywords.
}
\examples{
# Character data, or zero arguments
(x <- check_keywords())
stopifnot(length(x) > 1, is.character(x), is.null(names(x)))
(x <- check_keywords(c("aplot", "loess")))
stopifnot(length(x) == 2, is.character(x), names(x) == c("aplot", "loess"))

# 'Rd' objects
data(xyplot.docu)
(x <- check_keywords(xyplot.docu))
stopifnot(identical(x, "hplot"))
}
\seealso{
  Other rd-functions: \code{\link{check_examples}},
    \code{\link{repair_docu}},   }
\keyword{package}

