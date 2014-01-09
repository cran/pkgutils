\docType{class}
\name{DBTABLES-class}
\alias{DBTABLES}
\alias{DBTABLES-class}
\title{\code{DBTABLES} class}
\description{
  This virtual class is intended for holding, in each slot,
  a data frame that can be written to (or read from) a
  table in a relational database.
}
\details{
  The idea behind this class is to store the tables that
  are dependent on each other via foreign keys, in order.
  Based on a simple naming convention, methods check the
  supposed primary and the foreign keys (cross references)
  of the tables.

  Primary keys must be present in a column called
  \sQuote{id}, which must not contain duplicates. Columns
  with foreign keys must be named like \code{paste0(x,
  "_id")} and then refer to the column \sQuote{id} in a
  preceding slot named \code{paste0(x, "s")}. Irregular
  plurals are not currently understood. The two columns
  must be set-equal.

  For using another naming scheme for the keys, at least on
  of the methods \code{\link{pkeys}} and
  \code{\link{fkeys}} has to be overwritten in a child
  class, but none of the other methods. The same holds if
  slots should be included that are not treated as data
  base tables. \code{\link{split}} might then also need to
  be overwritten to not ignore these other slots when
  creating new objects of the class.

  For the methods of \code{DBTABLES}, see
  \code{\link{DBTABLES-methods}}.
}
\seealso{
  methods::Methods

  Other dbtables: \code{\link{DBTABLES-methods}},
  \code{\link{by}}, \code{\link{fkeys}},
  \code{\link{fkeys_valid}}, \code{\link{head}},
  \code{\link{pkeys}}, \code{\link{pkeys_valid}},
  \code{\link{sort}}, \code{\link{split}},
  \code{\link{summary}}, \code{\link{tail}},
  \code{\link{update}}
}
\keyword{classes}
\keyword{methods}

