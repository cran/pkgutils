\name{paper_size}
\alias{paper_size}
\alias{paper_size.character}
\alias{paper_size.numeric}
\title{Paper size}
\usage{
  paper_size(x, ...)

  \method{paper_size}{numeric} (x, landscape = FALSE,
    series = c("A", "B", "C"), ...)

  \method{paper_size}{character} (x, landscape = FALSE,
    inches = FALSE, ...)
}
\arguments{
  \item{x}{Numeric or character vector. If a numeric
  vector, specifying paper sizes in the \sQuote{DIN}
  series. If a character vector, the full names of
  well-known paper formats such as \sQuote{a4},
  \sQuote{letter}, etc.}

  \item{landscape}{Logical scalar. If \code{FALSE},
  \sQuote{portrait} paper orientation is assumed. For the
  character method, this has only an effect for paper size
  specifiers such as \sQuote{letter} that do not belong to
  the \sQuote{DIN} series. For the \sQuote{DIN} series,
  append \sQuote{R} to the specifier to obtain
  \sQuote{landscape} orientation.}

  \item{inches}{Logical scalar. If \code{TRUE}, output unit
  is inches, otherwise millimeters.}

  \item{series}{Character scalar indicating the
  \sQuote{DIN} series to assume.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  Numeric matrix with columns \sQuote{width} and
  \sQuote{height} and \code{x} as row names (if ot was a
  character vector).
}
\description{
  Determine the size for a given standard paper format.
}
\details{
  The computation is done numerically for the the DIN
  series, whereas a lookup table is used for the other
  formats.
}
\examples{
query <- c("A4", "Letter", "unknown")
(x <- paper_size(query))
stopifnot(is.matrix(x), is.numeric(x), rownames(x) == query)
stopifnot(colnames(x) == c("height", "width"), is.na(x["unknown", ]))
(y <- paper_size(4))
stopifnot(identical(y, x[1L, , drop = FALSE]))
}
\references{
  \url{http://en.wikipedia.org/wiki/Paper_size}
}
\seealso{
  Other plotting-functions: \code{\link{max_rgb_contrast}},
    \code{\link{mypdf}}, }
\keyword{dplot}

