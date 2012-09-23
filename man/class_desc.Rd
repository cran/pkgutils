\name{class_desc}
\alias{class_desc}
\alias{class_desc.class_desc}
\alias{class_desc.list}
\alias{classes_desc}
\alias{classes_desc.classes_desc}
\alias{classes_desc.list}
\title{Create class-description objects}
\usage{
  class_desc(x)

  \method{class_desc}{list} (x)

  \method{class_desc}{class_desc} (x)

  classes_desc(x)

  \method{classes_desc}{list} (x)

  \method{classes_desc}{classes_desc} (x)
}
\arguments{
  \item{x}{\R object to be converted.}
}
\value{
  Object of class \sQuote{class_desc} or
  \sQuote{classes_desc}.
}
\description{
  Constructor functions for class-description objects.
  These are used for defining specific methods for the
  result of \code{promptClass} from the \pkg{methods}
  package.
}
\note{
  None of these objects is currently used by the
  \sQuote{docu.R} script that comes with the package.
}
\seealso{
  methods::promptClass
}
\keyword{internal}

