\name{subset}
\alias{subset}
\alias{subset.Rd}
\alias{subset.pack_desc}
\alias{subset.pack_descs}
\title{Subset an Rd or pack_desc object}
\usage{
  \method{subset}{Rd} (x, subset, values = FALSE, ...)

  \method{subset}{pack_desc} (x, ...)

  \method{subset}{pack_descs} (x, ...)
}
\arguments{
  \item{x}{Object of class \sQuote{Rd}, \sQuote{pack_desc}
  or \sQuote{pack_descs}.}

  \item{subset}{Character vector with names of attributes
  of elements to keep.}

  \item{values}{Logical scalar indicating whether list
  elements or a logical vector indicating the positions
  within \code{x} should be returned.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  Logical vector or object of class \sQuote{Rd}. For the
  \sQuote{pack_descs} and \sQuote{pack_desc} method, a
  (nested) list.
}
\description{
  Create a subset of an \sQuote{Rd} object based on the
  attributes of its elements. The \sQuote{pack_desc} method
  reduces the input object to a list containing certain
  relevant components.
}
\keyword{internal}

