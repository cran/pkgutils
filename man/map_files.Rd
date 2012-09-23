\name{map_files}
\alias{map_files}
\alias{map_files.character}
\title{Map files}
\usage{
  map_files(x, ...)

  \method{map_files}{character} (x, mapfun, ...,
    .attr = ".filename", .encoding = "unknown")
}
\arguments{
  \item{x}{Character vector of input (and potentially
  output) file names.}

  \item{mapfun}{Mapping function, receives character vector
  with the lines per file as first argument, with the name
  of the file added as attribute with the name given using
  \code{.attr}.}

  \item{...}{Optional additional arguments passed to
  \code{fun}.}

  \item{.attr}{Character scalar. See description to
  \code{mapfun}.}

  \item{.encoding}{Passed to \code{readLines} as
  \sQuote{encoding} argument.}
}
\value{
  Logical vector using \code{x} as names, with \code{TRUE}
  indicating a successfully modified file, \code{FALSE} a
  file that yielded no errors but needed not to be
  modified, and \code{NA} a filename that caused an error.
  An attribute \sQuote{errors} is provided, containing a
  character vector with error messages (empty strings if no
  error occurred).
}
\description{
  Read lines from a file, modify the lines using a given
  function, and write the lines back to the input file
  unless the result of applying the function is identical
  to the lines read.
}
\details{
  If \code{mapfun} returns \code{NULL}, it is ignored.
  Otherwise is it an error if \code{mapfun} does not return
  a character vector. If this vector is identical to the
  lines read from the file, it is not printed to this file.
  Otherwise the file is attempted to be overwritten with
  the result of \code{mapfun}.
}
\examples{
tmpfile <- tempfile()
write(letters, file = tmpfile)
(x <- map_files(tmpfile, identity))
stopifnot(!x)
(x <- map_files(tmpfile, toupper))
stopifnot(x)
x <- readLines(tmpfile)
stopifnot(x == LETTERS)
(x <- map_files(tmpfile, as.null))
stopifnot(!x)
}
\seealso{
  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{logfile}},   \code{\link{must}},
  \code{\link{run_ruby}},     \code{\link{sections}},
    }
\keyword{IO}
