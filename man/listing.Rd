\name{listing}
\alias{listing}
\alias{listing.character}
\alias{listing.default}
\alias{listing.double}
\alias{listing.list}
\title{Nicer message listings}
\usage{
  listing(x, ...)

  \method{listing}{double} (x, ...)

  \method{listing}{default} (x, ...)

  \method{listing}{list} (x, ...)

  \method{listing}{character} (x, header = NULL,
    footer = NULL, prepend = FALSE, style = "list",
    collapse = if (style == "sentence") "" else "\n",
    force.numbers = FALSE,
    last.sep = c("and", "both", "comma"), ...)
}
\arguments{
  \item{x}{For the default method, an object convertible
  via \code{unlist} to a vector. In the default style mode,
  its \sQuote{names} attribute is used as the first column
  of the resulting listing; if it is \code{NULL} or if
  \code{force.numbers} is \code{TRUE}, numbers are
  inserted. The \sQuote{double} method is controlled by the
  \sQuote{digits} entry of \code{options} from the
  \pkg{base} package.}

  \item{header}{\code{NULL} or character vector. Prepended
  to the result.}

  \item{footer}{\code{NULL} or character vector. Appended
  to the result.}

  \item{prepend}{Logical, numeric or character scalar.
  Prepended to each line except \code{header} and
  \code{footer}. If numeric, the number of spaces.
  \code{TRUE} causes tabs to be used, \code{FALSE} turns
  prepending off. If in \sQuote{character} mode, used
  directly. The behaviour is distinct if \code{style} is
  \code{sentence}. In that case, a logical scalar decides
  about whether names are prepended before joining the
  vector elements, and a character scalar is used as
  template for \code{sprintf}}

  \item{style}{Character scalar. If \sQuote{table} or
  \sQuote{list}, passed to \code{formatDL}. If
  \sQuote{sentence}, a comma-separated list is created from
  \code{x}, the last separator according to
  \code{last.sep}.  Otherwise, a template for
  \code{sprintf} is assumed taking two arguments, the names
  of \code{x} and the values \code{x}.}

  \item{collapse}{Character scalar used to join the
  resulting vector elements.  Has no effect unless
  \code{header} or \code{footer} is given.}

  \item{force.numbers}{Logical scalar. Always use numbers
  instead of the \sQuote{names} attribute?}

  \item{last.sep}{Character scalar indicating what should
  be used as last separator if \code{style} is
  \sQuote{sentence}. Ignored unless the length of \code{x}
  is at least 3.}

  \item{...}{Optional other arguments passed to
  \code{formatDL}.}
}
\value{
  Character scalar.
}
\description{
  Create some kind of listing, used, e.g., in (error)
  messages or warnings.
}
\examples{
# default style
x <- structure(letters[1:5], names = LETTERS[1:5])
(y <- listing(x, "Five letters:", "...end here", 1))
stopifnot(length(y) == 1, y ==
  "Five letters:\n A: a\n B: b\n C: c\n D: d\n E: e\n...end here")

# 'sentence' style
(y <- listing(letters[1:3], style = "sentence", last.sep = "both"))
stopifnot(y == "a, b, and c", length(y) == 1)
(y <- listing(letters[1:3], style = "sentence", prepend = TRUE))
stopifnot(y == "1: a, 2: b and 3: c", length(y) == 1)
(y <- listing(letters[1:3], style = "sentence", prepend = "\%s=>\%s"))
stopifnot(y == "1=>a, 2=>b and 3=>c", length(y) == 1)

# with explicit sprintf template
(y <- listing(x, style = "\%s, \%s", collapse = "; ", prepend = "!"))
stopifnot(y == "!A, a; !B, b; !C, c; !D, d; !E, e", length(y) == 1)

# other 'x' arguments
stopifnot(listing(x) == listing(as.list(x)))
stopifnot(listing(pi) == "1: 3.141593") # controlled by getOption("digits")
}
\seealso{
  base::message base::warning base::stop base::formatDL

  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{logfile}},   \code{\link{map_files}},
  \code{\link{must}},
  \code{\link{run_ruby}},     \code{\link{sections}},
    }
\keyword{utilities}

