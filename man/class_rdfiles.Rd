\name{class_rdfiles}
\alias{class_rdfiles}
\alias{class_rdfiles.character}
\title{Dump or name class RD files}
\usage{
  class_rdfiles(pkg, ...)

  \method{class_rdfiles}{character} (pkg, classes,
    what = c("dump", "update", "name", "content"), ...)
}
\arguments{
  \item{classes}{Character vector with the names of classes
  of interest.}

  \item{what}{Character scalar determing the main mode of
  action.}

  \item{...}{Optional arguments passed to
  \code{\link{update}}. Ignored unless \code{what} is
  \sQuote{update}.}

  \item{pkg}{Name(s) of one to several package directories.
  The package name alone does \strong{not} suffice unless
  the package is a subdirectory of the working directory.}
}
\value{
  List of \code{promptClass} results, returned invisibly.
}
\description{
  Create RD files for the classes belonging to a given
  package. This is based on \code{promptClass} from the
  \pkg{methods} package. Alternatively, just return the
  names of the class.
}
\seealso{
  methods::promptClass
}
\keyword{internal}

