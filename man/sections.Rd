\name{sections}
\alias{sections}
\alias{sections.character}
\alias{sections.logical}
\title{Create sections}
\usage{
  sections(x, ...)

  \method{sections}{logical} (x, include = TRUE, ...)

  \method{sections}{character} (x, pattern, invert = FALSE,
    include = TRUE, perl = TRUE, ...)
}
\arguments{
  \item{x}{Logical vector. It is an error if \code{NA}
  values are contained.}

  \item{include}{Logical scalar indicating whether the
  separator positions should also be included in the factor
  levels instead of being coded as \code{NA}.}

  \item{pattern}{Scalar. If of mode \sQuote{character},
  passed to \code{grepl} from the \pkg{base} package. If
  numeric, used to indicate the lengths of the substrings
  to extract.}

  \item{invert}{Negate the results of \code{grepl}?}

  \item{perl}{Logical scalar passed to \code{grepl}.}

  \item{...}{Optional arguments passed to and from other
  methods, or between the methods.}
}
\value{
  The \sQuote{logical} method returns an ordered factor,
  its length being the one of \code{x}. The levels
  correspond to a groups whose indices correspond to the
  index of a \code{TRUE} value in \code{x} plus the indices
  of the \code{FALSE} values immediately following it.
  \sQuote{logical} method returns a list of character
  vectors.
}
\description{
  The \sQuote{logical} method treats a logical vector by
  regarding \code{TRUE} as indicating separation. It
  creates a factor that could be used with \code{split} to
  split the logical vector, or any equal-length object from
  which it was created, into according groups. The
  \sQuote{character} method splits a character vector
  according to a pattern or to a given output substring
  length.
}
\details{
  When applying \code{split}, positions corresponding to
  \code{NA} factor levels will usually be removed. Thus
  note the action of the \code{include} argument, and note
  that the positions of \code{TRUE} values that are
  followed by other \code{TRUE} values are always set to
  \code{NA}, irrespective of \code{include}. The
  \sQuote{character} method using a pattern works by
  passing this pattern to \code{grepl}, the result to the
  \sQuote{logical} method and this in turn to \code{split}.
}
\examples{
## 'logical' method

# clean input
x <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
(y <- sections(x))
stopifnot(identical(as.ordered(c(1, 1, 1, 2, 2, 3, 3)), y))

# now exclude the separators
y <- sections(x, include = FALSE)
stopifnot(identical(as.ordered(c(NA, 1, 1, NA, 2, NA, 3)), y))

# leading FALSE
x <- c(FALSE, x)
(y <- sections(x))
stopifnot(identical(as.ordered(c(1, 2, 2, 2, 3, 3, 4, 4)), y))

# adjacent TRUEs and trailing TRUE
x <- c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
(y <- sections(x))
stopifnot(identical(as.ordered(c(1, NA, 2, 2, 3, 3, NA, 4, 4, 5)), y))

# several adjacent TRUEs
x <- c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
(y <- sections(x))
stopifnot(identical(as.ordered(c(1, NA, NA, NA, 2, 2, 3, 3)), y))

## 'character' method

# using a specified length
x <- c("abcdef", "ghijk")
(y <- sections(x, 2))
stopifnot(is.list(y), length(y) == 2)
stopifnot(y[[1]] == c("ab", "cd", "ef"), y[[2]] == c("gh", "ij", "k"))

# using a regexp pattern
x <- c(">abc", ">def", "acgtagg", ">hij", "gatattag", "aggtagga") # FASTA
(y <- sections(x, "^>", include = TRUE))
stopifnot(identical(y, list(`1` = x[2:3], `2` = x[4:6])))
(y <- sections(x, "^>", include = FALSE))
stopifnot(identical(y, list(`1` = x[3], `2` = x[5:6])))
}
\seealso{
  base::split base::grepl

  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{flatten}},   \code{\link{listing}},       \code{\link{logfile}},   \code{\link{map_files}},
  \code{\link{must}},
  \code{\link{run_ruby}},     }
\keyword{utilities}

