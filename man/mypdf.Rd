\name{mypdf}
\alias{max_rgb_contrast}
\alias{max_rgb_contrast.default}
\alias{mypdf}
\alias{mypdf.character}
\alias{paper_size}
\alias{paper_size.character}
\alias{paper_size.numeric}
\title{Paper size for PDF files}
\usage{
  mypdf(file, ...)

  \method{mypdf}{character} (file, paper = "a4r",
    prop = 0.9, ...)

  paper_size(x, ...)

  \method{paper_size}{numeric} (x, landscape = FALSE,
    series = c("A", "B", "C"), ...)

  \method{paper_size}{character} (x, landscape = FALSE,
    inches = FALSE, ...)

  max_rgb_contrast(x, ...)

  \method{max_rgb_contrast}{default} (x, ...)
}
\arguments{
  \item{file}{Passed as \sQuote{file} argument to
  \code{pdf} from the \pkg{grDevices} package. See there
  for details.}

  \item{paper}{Character scalar like the eponymous argument
  of \code{pdf}, but here it is passed to
  \code{\link{paper_size}} to determine the \sQuote{width}
  and the \sQuote{height} of the plotting region.}

  \item{prop}{Numeric vector. Its values should be between
  0 and 1. Its first element is multiplied with the width
  of \code{paper} to yield the width of the plotting
  region. Its last element is multiplied with the height of
  \code{paper} to yield the height of the plotting region.}

  \item{x}{Numeric or character vector for determining the
  paper size. If a numeric vector, specifying paper sizes
  in the \sQuote{DIN} series. If a character vector, the
  full names of well-known paper formats such as
  \sQuote{a4}, \sQuote{letter}, etc.

  For \code{max_rgb_contrast}, the names or hexadecimal
  codes of the colours to be sorted. Might also be an
  integer vector, see \code{col2rgb} from the
  \pkg{grDevices} package for details. Duplicate RGB
  coordinates and unknown names will cause an error.}

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

  \item{...}{Optional arguments passed to other methods,
  e.g., to \code{pdf} from the \pkg{grDevices} package.}
}
\value{
  \code{mypdf} returns \code{NULL}. As a side effect,
  \code{file} is opened. \code{paper_size} yields a numeric
  matrix with columns \sQuote{width} and \sQuote{height}
  and \code{x} as row names (if it was a character vector).
  \code{max_rgb_contrast} returns a character vector (the
  rearranged input names).
}
\description{
  \code{mypdf} is a wrapper for \code{pdf} from the
  \pkg{grDevices} package. The difference is that
  \code{mypdf} determines the width and the height of the
  plotting region from a given standard paper format using
  \code{paper_size}. \code{max_rgb_constrast} is a helper
  functions for arranging colours.
}
\details{
  The computation of the paper size is done numerically for
  the \acronym{DIN} series, whereas a lookup table is used
  for the other formats.

  \code{max_rgb_contrast} arranges colours so as to achieve
  that neighboring colours are most distinct with respect
  to their RGB coordinates. This is done as follows: (1)
  Euclidean distances between the RGB coordinates of the
  input colours are calculated; (2) the distances are
  logarithmized and inversed; (3) a principal-coordinate
  analysis is conducted on these inversed distances; (4)
  the input colours are sorted according to the first
  principal coordinate.

  Note that this is probably only works for colour vectors
  of small to moderate size, and that the resulting vector
  could as well be used in reverse order (see the
  examples).
}
\examples{
\dontrun{
  mypdf("example.pdf")
  ## create some plots...
  dev.off()
}

## paper_size()

query <- c("A4", "Letter", "unknown")
(x <- paper_size(query))
stopifnot(is.matrix(x), is.numeric(x), rownames(x) == query)
stopifnot(colnames(x) == c("height", "width"), is.na(x["unknown", ]))
(y <- paper_size(4))
stopifnot(identical(y, x[1L, , drop = FALSE]))

## max_rgb_contrast()

# with colours
(x <- max_rgb_contrast(c("darkred", "darkblue", "blue", "red")))
y <- c("darkblue", "red", "blue", "darkred")
stopifnot(identical(x, y) || identical(x, rev(y)))

# shades of grey 1
(x <- max_rgb_contrast(c("white", "grey", "black")))
y <- c("grey", "black", "white")
stopifnot(identical(x, y) || identical(x, rev(y)))

# shades of grey 2
(x <- max_rgb_contrast(c("white", "darkgrey", "lightgrey", "black")))
y <- c("lightgrey", "black", "white", "darkgrey")
stopifnot(identical(x, y) || identical(x, rev(y)))
}
\references{
  \url{http://en.wikipedia.org/wiki/Paper_size}
}
\seealso{
  grDevices::pdf grDevices::col2rg
}
\keyword{IO}
\keyword{color}
\keyword{dplot}

