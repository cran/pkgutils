\name{update}
\alias{update}
\alias{update.class_desc}
\alias{update.classes_desc}
\alias{update.numeric_version}
\alias{update.pack_desc}
\alias{update.pack_descs}
\title{Modify class- or package-description objects}
\usage{
  \method{update}{classes_desc} (object, ...)

  \method{update}{class_desc} (object,
    outcomment = c("note", "author", "references", "seealso"),
    description = sprintf("  See \%s.", rd_quote(c("code", "link"), class_name(object))),
    ...)

  \method{update}{pack_desc} (object, version = TRUE,
    date.format = "\%Y-\%m-\%d", ...)

  \method{update}{pack_descs} (object, ...)

  \method{update}{numeric_version} (object, ...)
}
\arguments{
  \item{object}{Object of class \sQuote{class_desc},
  \sQuote{classes_desc}, \sQuote{pack_desc} or
  \sQuote{pack_descs}.}

  \item{outcomment}{Character vector with the names of
  entries to be commented out.}

  \item{description}{A replacement for the
  \sQuote{description} entry of the class. Ignored if empty
  or if \sQuote{description} occurs in
  \sQuote{outcomment}.}

  \item{...}{For the \sQuote{class_desc} and
  \sQuote{classes_desc} methods, optional other named
  arguments to be set. Ignored if the name does not occur
  in the names of \code{object} or if it does occur in
  \code{outcomment} or if the value has zero length.}

  \item{version}{Logical scalar. Also update the version?
  Only relevant if \code{action} is \sQuote{update}. Note
  that this updating would only affect the last part of the
  version string separated by \sQuote{-}; if this does not
  exist, it is ignored. Updating is \strong{not} done if
  the old date is identical or newer to the new one. Note
  that this comparison only works properly if
  \code{data.format} is correctly specified.}

  \item{date.format}{Character scalar. The format used and
  expected for the date of the package.}
}
\value{
  \R object of the same class than \code{object}.
}
\description{
  Modify class-description objects by outcommenting and
  otherwise changing sections, or modify
  package-description objects by updating the date and
  optionally also the version.
}
\keyword{internal}

