\name{mypdf}
\alias{mypdf}
\alias{mypdf.character}
\title{Create PDF file}
\usage{
  mypdf(file, ...)

  \method{mypdf}{character} (file, paper = "a4r",
    prop = 0.9, ...)
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

  \item{...}{Optional arguments passed to \code{pdf} from
  the \pkg{grDevices} package.}
}
\value{
  \code{NULL}. As a side effect, \code{file} is opened.
}
\description{
  A wrapper for \code{pdf} from the \pkg{grDevices}
  package. The difference is that \code{mypdf} determines
  the width and the height of the plotting region from the
  paper format.
}
\examples{
\dontrun{
  mypdf("example.pdf")
  ## create some plots...
  dev.off()
}
}
\seealso{
  grDevices::pdf

  Other plotting-functions: \code{\link{max_rgb_contrast}},
    \code{\link{paper_size}},
    }
\keyword{IO}

