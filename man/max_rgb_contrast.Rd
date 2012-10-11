\name{max_rgb_contrast}
\alias{max_rgb_contrast}
\alias{max_rgb_contrast.default}
\title{Sort colours}
\usage{
  max_rgb_contrast(x, ...)

  \method{max_rgb_contrast}{default} (x, ...)
}
\arguments{
  \item{x}{Vector. Names or hexadecimal codes of the
  colours to be sorted.  Might also be an integer vector,
  see \code{col2rgb} from the \pkg{grDevices} package for
  details. Duplicate RGB coordinates and unknown names will
  cause an error.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  Character vector (rearranged input names).
}
\description{
  Arrange colours so as to achieve that neighboring colours
  are most distinct with respect to their RGB coordinates.
  This is done as follows: (1) Euclidean distances between
  the RGB coordinates of the input colours are calculated;
  (2) the distances are logarithmized and inversed; (3) a
  principal-coordinate analysis is conducted on these
  inversed distances; (4) the input colours are sorted
  according to the first principal coordinate.
}
\note{
  The resulting vector could as well be used in reverse
  order (see the examples).
}
\examples{
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
\seealso{
  grDevices::col2rgb

  Other plotting-functions: \code{\link{mypdf}},
  \code{\link{paper_size}},
    }
\keyword{color}

