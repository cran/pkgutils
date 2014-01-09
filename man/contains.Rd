\name{contains}
\alias{contains}
\title{Query an object with another object}
\arguments{
  \item{object}{List containing the data,.}

  \item{other}{List used as query.}

  \item{values}{Logical scalar. Compare also the values or
  only the keys? If \code{FALSE}, \code{exact} is ignored.}

  \item{exact}{Logical scalar. If \code{FALSE}, the data
  value(s) might by any of the query value(s), and some
  coercion is done before comparing (see \code{match} for
  details.

  If \code{TRUE}, the data value(s) must exactly correspond
  to the query value(s), and no coercion is done (see
  \code{identical}) for details). This might be too strict
  for most applications.}

  \item{...}{Optional arguments passed to \code{identical}
  from the \pkg{base} package, allowing for fine-control of
  identity. Has no effect unless \code{exact} is
  \code{TRUE}.}
}
\value{
  Logical scalar.
}
\description{
  One use is to test whether all names of a query list
  occur as names in a data list and optionally also whether
  they point to the same elements; this principle is
  applied recursively to all contained lists.
}
\details{
  Non-list elements are ignored if \code{values} is
  \code{FALSE}.  Otherwise the comparison is done using
  \code{identical} if \code{exact} is \code{TRUE}. If
  \code{exact} is \code{FALSE}, the value(s) in the data
  list can be any of the values at the corresponding
  position in the query list, and the comparison is done by
  coercion to character vectors. An empty query list
  results in \code{TRUE}. Missing names in a non-empty
  query list result in \code{FALSE}.
}
\examples{
# List/list method
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
stopifnot(contains(x, y))
stopifnot(!contains(x, y, exact = TRUE))
stopifnot(contains(x, y, exact = TRUE, values = FALSE))
}
\seealso{
  base::list base::as.list base::`[` base::`[[` base::match

  base::identity

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{collect}},   \code{\link{flatten}},   \code{\link{listing}},
          \code{\link{map_names}}, \code{\link{map_values}},
  \code{\link{must}}
}
\keyword{attribute}
\keyword{list}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{contains-methods}
\alias{contains,list,list-method}
\usage{
  \S4method{contains}{list,list}(object, other,
    values = TRUE, exact = FALSE, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
