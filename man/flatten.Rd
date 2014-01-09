\name{flatten}
\alias{collect}
\alias{collect.list}
\alias{flatten}
\alias{flatten.default}
\alias{flatten.list}
\title{Flatten a list or collect information from it}
\usage{
  flatten(object, ...)

  \method{flatten}{default} (object, ...)

  \method{flatten}{list} (object, use.names = TRUE, ...)

  collect(x, what, ...)

  \method{collect}{list} (x,
    what = c("counts", "occurrences", "values", "elements", "datasets"),
    min.cov = 1L, keep.unnamed = FALSE, dataframe = FALSE,
    optional = TRUE,
    stringsAsFactors = default.stringsAsFactors(), ...)
}
\arguments{
  \item{object}{Usually a list. The default method just
  returns \code{object} if it is atomic but raises an error
  otherwise.}

  \item{use.names}{Logical scalar passed to \code{unlist}
  from the \pkg{base} package.}

  \item{x}{List.}

  \item{what}{Character scalar indicating how to collect
  information.  \describe{ \item{counts}{For all non-list
  elements of \code{x}, count their occurrences.}
  \item{occurrences}{Like \sQuote{counts}, but only
  indicate presence or absence.} \item{values}{Simplify all
  direct elements of \code{x}, irrespective of whether or
  not they are lists, for including them as rows in a data
  frame.  Their names determine the columns. See
  \code{keep.unnamed} for the action in the case of missing
  names.} \item{elements}{Like \sQuote{elements}, but
  collect only the non-list elements of \code{x}, i.e.
  flatten \code{x} in the first step.}
  \item{datasets}{Convert all elements to data frames or
  matrices, then merge them using rows and column names. In
  case of conflict, the last ones win.  Here, the behaviour
  of other arguments is special if all elements of \code{x}
  are atomic. See below.} }}

  \item{min.cov}{Numeric scalar indicating the minimal
  coverage required in the resulting presence-absence
  matrix. Columns with a fewer number of non-zero entries
  are removed.}

  \item{keep.unnamed}{Logical scalar indicating whether
  names should be inserted for elements of \code{x} that
  miss them. If \code{NA}, they are skipped, but with a
  warning; if \code{FALSE}, they are skipped silently.
  This only has an effect in conjunction with the last
  three values of \code{what}. If \kbd{datasets} are
  chosen, it usually has only an effect if all elements of
  \code{x} are atomic.}

  \item{dataframe}{Logical scalar indicating whether a data
  frame should be produced instead of a matrix}

  \item{optional}{See \code{as.data.frame} from the
  \pkg{base} package.}

  \item{stringsAsFactors}{See \code{as.data.frame} from the
  \pkg{base} package.}

  \item{...}{Optional arguments passed to and from other
  methods (if requested to \code{as.data.frame}).}
}
\value{
  The list method of \code{flatten} returns a non-nested
  list. The \code{collect} methods yield a data frame or a
  matrix.
}
\description{
  Methods for making an object \sQuote{flat}, such as
  creating a non-nested list from a list, and methods for
  collecting information from list-like objects into a
  matrix or data frame.
}
\details{
  The list method of \code{flatten} is based on
  \url{http://stackoverflow.com/questions/8139677/} with
  some slight improvements.
}
\examples{
## flatten()
x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
  e = list(pi))
(y <- flatten(x)) # sublists removed, non-list elements kept
stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
# atomic objects are not modified by default
stopifnot(identical(letters, flatten(letters)))

## collect()
x <- list(X = list(A = 1:3, B = 7L, C = list('c1', 1:3)),
  Y = list(A = 1:3, 11, B = -1L, D = "?"))

## collect values into a data frame or matrix
(got <- collect(x, "values", dataframe = TRUE))
stopifnot(LETTERS[1:4] == colnames(got))
stopifnot(names(x) == rownames(got))
stopifnot(is.list(got$A), is.integer(got$B), is.list(got$C),
  is.factor(got$D))
stopifnot(!is.na(got$A), !is.na(got$B), any(is.na(got$C)), any(is.na(got$D)))
# include the unnamed ones
got <- collect(x, "values", dataframe = TRUE, keep.unnamed = TRUE)
stopifnot(dim(got) == c(2, 5))
# simplify to matrix
(got <- collect(x, "values", dataframe = FALSE))
stopifnot(is.matrix(got), mode(got) == "list")

## collect elements into a data frame or matrix
(got <- collect(x, "elements", dataframe = TRUE))
stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
  is.data.frame(got))
(got <- collect(x, "elements", dataframe = FALSE))
stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
  !is.data.frame(got))

## count or just note occurrences
(got <- collect(x, "counts", dataframe = FALSE))
stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
  setequal(colnames(got), unlist(x)), any(got > 1))
(got <- collect(x, "occurrences", dataframe = FALSE))
stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
  setequal(colnames(got), unlist(x)), !any(got > 1))

## convert to data frames and insert everything in a single one
(got <- collect(x, "datasets", optional = FALSE, dataframe = TRUE))
stopifnot(dim(got) == c(3, 6), is.data.frame(got))

## a more useful application is to merge matrices
m1 <- matrix(1:4, ncol = 2, dimnames = list(c("A", "B"), c("x", "y")))
m2 <- matrix(1:4, ncol = 2, dimnames = list(c("C", "B"), c("x", "z")))
(got <- collect(list(m1, m2), "datasets"))
# values missing in some matrix yield NA
stopifnot(dim(got) == c(3, 3), any(is.na(got)))
}
\seealso{
  base::unlist base::as.data.frame

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{contains}}, \code{\link{listing}},
          \code{\link{map_names}}, \code{\link{map_values}},
  \code{\link{must}}
}
\keyword{manip}

