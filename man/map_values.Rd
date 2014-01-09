\name{map_values}
\alias{map_names}
\alias{map_values}
\title{Map values or names}
\arguments{
  \item{object}{When mapping values, a list (may be
  nested), data frame or character vector. If it has names,
  they are preserved. \code{NULL} can also be given and
  yields \code{NULL} or an empty named character vector (if
  \code{mapping} is missing). \code{object} may also be an
  array (which includes matrices).

  When mapping names, \code{object} can be any \R object.
  The default method applies the mapping to the
  \sQuote{names} attribute. The behaviour is special for
  lists, which are traversed recursively to also consider
  contained lists with names. Data frames and array objects
  (that is, including matrices) are also treated specially
  because the \code{dimnames} attribute, not the
  \sQuote{names} attribute is considered.}

  \item{mapping}{When mapping values, a character vector,
  function, expression, \code{NULL} or missing.  \itemize{
  \item If a character vector used as a mapping from its
  names to its values.  Values from \code{object} are
  searched for in the \code{names} attribute of
  \code{mapping}; those found are replaced by the
  corresponding values of \code{mapping}.  \item If
  \code{mapping} is missing, a character vector is returned
  (sorted and with duplicates removed) whose names are
  identical to the values. This eases the construction of
  mapping vectors specific for \code{object}. If
  \code{mapping} is missing, the \code{coerce} argument
  must be named.  \code{mapping} changes its usage if
  \code{coerce} is \code{TRUE}.  \item For array objects,
  if \code{mapping} was a function, it would be applied to
  \code{object} after conversion with \code{as.vector}, and
  it would be attempted to add the original attributes
  (particularly important are \sQuote{dim} and
  \code{dimnames} back to the result.  \item For array
  objects, if \code{mapping} is the usual character vector,
  it then is used for mapping the \code{storage.mode}, not
  the \code{class} of \code{object}.  \item If
  \code{mapping} is an expression, all sub-expressions will
  be evaluated in \code{object} represented as an
  environment, which after conversion back to a list, is
  returned.  \item If \code{mapping} is \code{NULL} and
  \code{object} is a list, all contained objects of zero
  length are removed recursively.  }

  When mapping names, a mapping function that takes a
  character vector as first argument, or character vector
  used for mapping from its names to its values, or
  missing. It is guaranteed that \code{NULL} input remains
  \code{NULL}, irrespective of the value of
  \code{mapping}.}

  \item{coerce}{The usage of this argument depends on
  \code{object}.  \itemize{ \item A character vector with
  the names of classes that are coerced to
  \sQuote{character} to allow the mapping. Other classes
  are returned unchanged. Note that the coerced data are
  \strong{not} converted back to their original data type.
  \sQuote{ANY} can be used to indicate that all classes
  will be considered.  \item Alternatively, \code{coerce}
  can be \code{TRUE}. \code{mapping} is then interpreted as
  a mapping between the names of classes, and \code{as}
  from the \pkg{methods} package is used for conducting the
  requested coercions. Attempting an undefined coercion
  will result in an error.  \item For the expression
  method, an enclosing environment to look up objects that
  are not found in \code{mapping}.  }}

  \item{...}{Optional further arguments to \code{mapping}
  (\strong{if} it is a function).}
}
\value{
  \code{map_values} returns a list, data frame, a character
  or logical vector or \code{NULL}.

  \code{map_names} yields a character vector if
  \code{mapping} is missing, otherwise an \R object of the
  same class than \code{object}.
}
\description{
  Map values using a character vector, a function or an
  expression, or use a character vector or a function for
  recursively mapping list names, or mapping the
  \code{colnames} and \code{rownames} attributes of a data
  frame or matrix.
}
\details{
  Mapping of \sQuote{character} data using another
  \sQuote{character} vector is possible, as well as
  recursively applying a mapping function to all
  \sQuote{character} values within a list, or
  non-recursively to a data frame. Optionally other data
  types are coerced to \sQuote{character}; the remaining
  ones are returned unchanged. It is also possible to map
  between classes using coercion functions. For convenience
  in programming, methods for the \sQuote{NULL} class are
  also available.

  Mapping of logical vectors using another vector expects
  (at least) three elements within the mapping vector, i.e.
  the values to be used for \code{FALSE}, \code{NA} and
  \code{TRUE} elements in \code{object}. Nothing is
  modified if the mapping is \code{NULL}. The default
  mapping vector \code{c(1L, 2L, 3L)} is used if
  \code{mapping} is missing.

  In the case of lists, the function passed to
  \code{map_names} is not applied to list elements which
  are not themselves lists, even if they have a
  \sQuote{names} attribute. Such elements and their names,
  if any, are returned unchanged. If a \sQuote{names},
  \code{colnames} or \code{rownames} attribute is
  \code{NULL}, it is ignored.

  Alternatively, instead of mapping the names, collect them
  and return them as a single character vector, sorted and
  with duplicates removed. The collected names are added as
  their own \code{names} attribute; this might be useful if
  the result is later on used for some mapping (using this
  function or \code{\link{map_values}}).
}
\examples{
## map_values()

# Character/character method
map <- letters
names(map) <- rev(LETTERS)
(x <- map_values(LETTERS, map))
stopifnot(rev(x) == letters)

# Character/missing method
(x <- map_values(letters))
stopifnot(x == letters, names(x) == letters)

# Character/function method
x <- letters[1:4]
names(x) <- LETTERS[1:4]
(y <- map_values(x, function(z) sprintf("\%s\%s", z, z)))
stopifnot(names(y) == names(x), y != x)

# List/character method
x <- list(a = 1:8, c = 9, d = 'x')
map <- c(a = "b", e = "f", x = "y")
(y <- map_values(x, map))
stopifnot(identical(x[1:2], y[1:2]), !identical(x[3], y[3]))
(y <- map_values(x, map, coerce = "integer"))
stopifnot(identical(x[2], y[2]), !identical(x[1], y[1]),
  !identical(x[3], y[3]))
(y <- map_values(x, map, coerce = TRUE))
stopifnot(identical(x, y))
(y <- map_values(x, c(numeric = "character"), coerce = TRUE))
stopifnot(identical(x[1], y[1]), !identical(x[2], y[2]),
  identical(x[3], y[3]))

# List/function method
(y <- map_values(x, identity, coerce = "ANY"))
stopifnot(identical(x, y))
(y <- map_values(x, class, coerce = "ANY"))
stopifnot(sapply(y, class) == "character", names(y) == names(x))

# List/missing method
(y <- map_values(x))
stopifnot(y == "x", names(y) == y)
(y <- map_values(x, coerce = "integer"))
stopifnot(length(y) == 9, names(y) == y)
(y <- map_values(x, coerce = c("integer", "numeric")))
stopifnot(length(y) == 10, names(y) == y)
(y <- map_values(x, coerce = "ANY")) # same effect
stopifnot(length(y) == 10, names(y) == y)
(y <- map_values(x, coerce = TRUE))
stopifnot(y == c("character", "integer", "numeric"), names(y) == y)

# List/expression method
(y <- map_values(x, expression(b <- a + c)))
stopifnot(is.list(y), y$b == c(10:17))

# List/expression method applied to a data frame
x <- data.frame(a = 1:5, b = 6:10)
(y <- map_values(x, expression(c <- a + b)))
stopifnot(is.data.frame(y), dim(y) == c(5, 3))

# Data frame/character method
x <- data.frame(a = 1:3, b = letters[1:3])
stopifnot(sapply(x, class) == c("integer", "factor"))
map <- c(a = "A", b = "B", c = "C", `1` = "5")
(y <- map_values(x, map))
stopifnot(identical(x, y))
(y <- map_values(x, map, coerce = "factor"))
stopifnot(!identical(x, y), y[[2]] == c("A", "B", "C"))
(y <- map_values(x, map, coerce = "ANY"))
stopifnot(y[[1]] == c("5", "2", "3"), y[[2]] == c("A", "B", "C"))
(y <- map_values(x, map, coerce = TRUE))
stopifnot(identical(x, y))
map <- c(factor = "character", integer = "complex")
(y <- map_values(x, map, coerce = TRUE))
stopifnot(sapply(y, class) == c("complex", "character"))

# Data frame/function method
(y <- map_values(x, `+`, coerce = "integer", y = 1L))
stopifnot(y$a == x$a + 1L)
(y <- map_values(x, as.character, coerce = "factor"))
stopifnot(sapply(y, class) == c("integer", "character"))

# Data frame/missing method
(y <- map_values(x))
stopifnot(is.character(y), length(y) == 0)
(y <- map_values(x, coerce = "factor"))
stopifnot(is.character(y), y == letters[1:3], names(y) == y)
(y <- map_values(x, coerce = "ANY"))
stopifnot(is.character(y), length(y) == 6, names(y) == y)
(y <- map_values(x, coerce = TRUE))
stopifnot(is.character(y), y == c("factor", "integer"), names(y) == y)

# Matrix/character method
(x <- matrix(1:6, nrow = 2))
(y <- map_values(x, c(integer = "numeric"), coerce = TRUE))
stopifnot(storage.mode(x) != storage.mode(y))
(y <- map_values(x, c(`1` = "7"), coerce = "integer"))
stopifnot(is.character(y), y[-1] == x[-1])

# Matrix/function method
(y <- map_values(x, identity))
stopifnot(identical(x, y))
(y <- map_values(x, `+`, y = 1)) # useless because '+' is directly available
stopifnot(dim(y) == dim(x), y == x + 1)

# Matrix/missing method
(y <- map_values(x))
stopifnot(y == "integer", names(y) == y)
(y <- map_values(x, coerce = "ANY"))
stopifnot(is.character(y), y == 1:6, names(y) == y)

# Factor/function method
x <- as.factor(c("a", "b", "a"))
(y <- map_values(x, toupper))
stopifnot(is.factor(y), y == toupper(x))

# Factor/character method
(y <- map_values(x, c(b = "c", k = "h")))
stopifnot(is.factor(y), levels(y) == c("a", "c"))

# Factor/missing method
(y <- map_values(x))
stopifnot(levels(x) == y, names(y) == y)

## map_names()

# List/function method
x <- list(a = 1:8, c = 9, d = 'x')
map <- function(x) sprintf("\%s\%s", x, x)
(y <- map_names(x, map))
stopifnot(identical(as.character(x), as.character(y)))
stopifnot(!identical(names(x), names(y)))

# List/character method
x <- list(a = 1:8, c = 9, d = 'x')
map <- c(a = "b", e = "f", x = "y")
(y <- map_names(x, map))
stopifnot(identical(as.character(x), as.character(y)))
stopifnot(!identical(names(x), names(y)))
# compare with the map_values() example

# List/missing method
x <- list(a = 1:8, c = 9, d = 'x')
(y <- map_names(x))
stopifnot(identical(as.vector(y), names(x)))
stopifnot(identical(names(y), names(x)))
# Now a recursive list
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
(y <- map_names(x))
stopifnot(length(y) > length(names(x)))

# Data frame/function method
x <- data.frame(a = 1:3, b = letters[1:3])
(y <- map_names(x, toupper))
stopifnot(identical(y[[1]], x[[1]]), identical(y[[2]], x[[2]]))
stopifnot(identical(names(y), c("A", "B")))

# Data frame/character method
(y <- map_names(x, c(a = "b", b = "a")))
stopifnot(x == y, names(y) == c("b", "a"))

# Data frame/missing method
(y <- map_names(x))
stopifnot(is.character(y), y == names(y), length(y) == 5)

# Matrix/function method
x <- as.matrix(x)
(y <- map_names(x, toupper))
stopifnot(x == y, toupper(colnames(x)) == colnames(y))

# Matrix/character method
(y <- map_names(x, c(a = "b", b = "a")))
stopifnot(x == y, colnames(y) == c("b", "a"))

# Matrix/missing method
(y <- map_names(x))
stopifnot(y == c("a", "b"), names(y) == y)
}
\seealso{
  base::rapply base::list base::as.list methods::as
  base::class base::storage.mode base::as.vector

  Other coding-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{collect}},   \code{\link{contains}}, \code{\link{flatten}},
    \code{\link{listing}},
          \code{\link{must}}
}
\keyword{list}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{map_names-methods}
\alias{map_names,ANY,character-method}
\alias{map_names,ANY,function-method}
\alias{map_names,ANY,missing-method}
\alias{map_names,array,character-method}
\alias{map_names,array,function-method}
\alias{map_names,array,missing-method}
\alias{map_names,data.frame,character-method}
\alias{map_names,data.frame,function-method}
\alias{map_names,data.frame,missing-method}
\alias{map_names,list,character-method}
\alias{map_names,list,function-method}
\alias{map_names,list,missing-method}
\alias{map_values-methods}
\alias{map_values,NULL,character-method}
\alias{map_values,NULL,function-method}
\alias{map_values,NULL,missing-method}
\alias{map_values,array,character-method}
\alias{map_values,array,function-method}
\alias{map_values,array,missing-method}
\alias{map_values,character,NULL-method}
\alias{map_values,character,character-method}
\alias{map_values,character,function-method}
\alias{map_values,character,missing-method}
\alias{map_values,data.frame,NULL-method}
\alias{map_values,data.frame,character-method}
\alias{map_values,data.frame,function-method}
\alias{map_values,data.frame,missing-method}
\alias{map_values,factor,character-method}
\alias{map_values,factor,function-method}
\alias{map_values,factor,missing-method}
\alias{map_values,list,NULL-method}
\alias{map_values,list,character-method}
\alias{map_values,list,expression-method}
\alias{map_values,list,function-method}
\alias{map_values,list,missing-method}
\alias{map_values,logical,NULL-method}
\alias{map_values,logical,function-method}
\alias{map_values,logical,missing-method}
\alias{map_values,logical,vector-method}
\usage{
  \S4method{map_names}{ANY,character}(object, mapping) 
  \S4method{map_names}{ANY,function}(object, mapping, ...) 
  \S4method{map_names}{ANY,missing}(object) 
  \S4method{map_names}{array,character}(object, mapping) 
  \S4method{map_names}{array,function}(object, mapping, ...) 
  \S4method{map_names}{array,missing}(object) 
  \S4method{map_names}{data.frame,character}(object, mapping) 
  \S4method{map_names}{data.frame,function}(object, mapping,
    ...) 
  \S4method{map_names}{data.frame,missing}(object) 
  \S4method{map_names}{list,character}(object, mapping) 
  \S4method{map_names}{list,function}(object, mapping, ...) 
  \S4method{map_names}{list,missing}(object) 

  \S4method{map_values}{NULL,character}(object, mapping) 
  \S4method{map_values}{NULL,function}(object, mapping, ...) 
  \S4method{map_values}{NULL,missing}(object, mapping) 
  \S4method{map_values}{array,character}(object, mapping,
    coerce = TRUE) 
  \S4method{map_values}{array,function}(object, mapping, ...) 
  \S4method{map_values}{array,missing}(object, coerce = TRUE) 
  \S4method{map_values}{character,NULL}(object, mapping) 
  \S4method{map_values}{character,character}(object, mapping) 
  \S4method{map_values}{character,function}(object, mapping,
    ...) 
  \S4method{map_values}{character,missing}(object) 
  \S4method{map_values}{data.frame,NULL}(object, mapping,
    coerce = character(), ...) 
  \S4method{map_values}{data.frame,character}(object, mapping,
    coerce = character()) 
  \S4method{map_values}{data.frame,function}(object, mapping,
    coerce = character(), ...) 
  \S4method{map_values}{data.frame,missing}(object,
    coerce = character()) 
  \S4method{map_values}{factor,character}(object, mapping) 
  \S4method{map_values}{factor,function}(object, mapping,
    ...) 
  \S4method{map_values}{factor,missing}(object) 
  \S4method{map_values}{list,NULL}(object, mapping,
    coerce = character()) 
  \S4method{map_values}{list,character}(object, mapping,
    coerce = character()) 
  \S4method{map_values}{list,expression}(object, mapping,
    coerce = parent.frame()) 
  \S4method{map_values}{list,function}(object, mapping,
    coerce = character(), ...) 
  \S4method{map_values}{list,missing}(object, mapping,
    coerce = character()) 
  \S4method{map_values}{logical,NULL}(object, mapping) 
  \S4method{map_values}{logical,function}(object, mapping,
    ...) 
  \S4method{map_values}{logical,missing}(object) 
  \S4method{map_values}{logical,vector}(object, mapping) 
}
%% END INSERTION BY repair_S4_docu.rb
