\name{source_files}
\alias{source_files}
\alias{source_files.character}
\alias{source_files.pack_desc}
\alias{source_files.pack_descs}
\title{Load R files with source}
\usage{
  source_files(x, ...)

  \method{source_files}{character} (x, ...)

  \method{source_files}{pack_descs} (x, ...)

  \method{source_files}{pack_desc} (x, demo = FALSE, ...)
}
\arguments{
  \item{x}{Character vector of file names, or object of
  class \sQuote{pack_desc} or \sQuote{pack_descs}.}

  \item{demo}{Logical scalar. See \code{\link{pack_desc}}.}

  \item{...}{Optional additional arguments passed between
  the methods and finally to \code{sys.source}.}
}
\value{
  List of lists with the results of calling \code{source}.
  For the action of the \sQuote{pack_desc} or
  \sQuote{pack_descs} methods, see \code{\link{pack_desc}}.
}
\description{
  Load \R code files with \code{source} from the \pkg{base}
  package (which does only handle one file at a time). This
  is mainly useful for loading entire packages with
  \code{source}; the the \sQuote{what} argument of
  \code{\link{pack_desc}} for an example.
}
\keyword{internal}

