\docType{data}
\name{xyplot_docu}
\alias{xyplot_docu}
\title{Example RD dataset: xyplot documentation}
\format{\sQuote{Rd} object as produced by \code{parse_Rd} from the
  \pkg{tools} package.}
\description{
  This \sQuote{Rd} object contains the description in
  \sQuote{Rd} format of \code{xyplot} from the
  \pkg{lattice} package, version 0.20-10.
}
\examples{
data(xyplot_docu)
stopifnot(inherits(xyplot_docu, "Rd"), is.list(xyplot_docu))
}
\keyword{datasets}
\keyword{datasets}

