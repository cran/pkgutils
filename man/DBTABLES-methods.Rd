\name{DBTABLES-methods}
\alias{DBTABLES-methods}
\alias{by}
\alias{fkeys}
\alias{fkeys_valid}
\alias{head}
\alias{pkeys}
\alias{pkeys_valid}
\alias{sort}
\alias{split}
\alias{summary}
\alias{tail}
\alias{update}
\title{Methods for \code{DBTABLES} objects}
\arguments{
  \item{object}{\code{\link{DBTABLES}} object.}

  \item{x}{\code{\link{DBTABLES}} object.}

  \item{data}{\code{\link{DBTABLES}} object.}

  \item{decreasing}{Logical scalar. If missing,
  \code{FALSE} is used instead.}

  \item{start}{Numeric vector or \code{NULL}. If a numeric
  vector and named, the names must be the slot names;
  otherwise these are inserted, in order.  Each value of
  \code{start} defines an new starting point (minimum) for
  the primary key in the respective table. \code{update}
  takes care that the foreign keys referring to this table
  are modified in the same way.

  If \code{NULL}, \code{start} is set to a vector that
  causes all output primary keys to start at 1. This can be
  used to revert a previous call of \code{update}.}

  \item{recursive}{Logical scalar passed from \code{c} as
  \code{drop} argument to \code{update}. Also causes
  \code{update} to be applied to \code{x} (and not only to
  the elements of \code{...}, if any).}

  \item{f}{Missing or (list of) factor(s) that fit(s) to
  the dimensions of the first table returned by. If
  missing, the primary key of this table is used.}

  \item{drop}{For \code{update}, a logical scalar that
  indicates whether or not the row names of all tables
  should be set to \code{NULL}.

  For \code{split}, \code{drop} is either missing or a
  logical scalar.  \code{TRUE} is the default. If so, after
  splitting, \code{update} is applied to each
  \code{\link{DBTABLES}} object that is an element of the
  resulting list, with \code{start} set to \code{NULL} and
  \code{drop} set to \code{TRUE}.}

  \item{INDICES}{If \code{do_inline} is \code{FALSE}, a
  vector used for selecting a subset of the tables and
  modifying their order (which might not make sense, see
  \code{\link{DBTABLES}}). Otherwise usually a vector of
  primary keys for the first table. Selection of rows from
  subsequent tables works via the defined foreign keys.
  Thus, used for querying for data slots for a novel
  \code{\link{DBTABLES}} with \code{FUN}.}

  \item{FUN}{Function to be applied to each table in turn.
  If \code{do_inline} is \code{FALSE}, it should accept a
  character scalar (name of a table) a data frame (this
  table) and as the first two (unnamed) arguments.

  If \code{do_inline} is \code{TRUE} and \code{simplify} is
  \code{FALSE}, \code{FUN} should accept three first
  arguments (in addition to those in \code{...}, if any),
  in order: the name of a table (database table or data
  frame), the name of a column in that table, and a vector
  of numeric indexes to match that column. It should return
  a data frame with all matched rows.

  If \code{do_inline} is \code{TRUE} and \code{simplify} is
  \code{TRUE}, \code{FUN} should accept a character scalar
  (\acronym{SQL} statement) as single first argument in
  addition to those in \code{...}, and use this to retrieve
  columns from a database table. The generated
  \acronym{SQL} uses double-quoting for the table and
  column identifiers and single-quoting for the query
  values (starting with \code{INDICES}) unless they are
  numeric.}

  \item{...}{Objects of the same class as \code{x} for
  \code{c}, optional further arguments of \code{FUN} for
  \code{by}.}

  \item{do_map}{Optional vector for mapping the slot names
  in \code{data} before passing them to \code{FUN}.}

  \item{do_inline}{Logical scalar indicating how \code{FUN}
  should be used and which kind of return value should be
  produced.  See \code{FUN} for details.}

  \item{do_quote}{Character scalar or function used for
  generating \acronym{SQL} identifiers. If a function, used
  directly; otherwise used as quote character (to be
  doubled within the character string).}

  \item{simplify}{Logical scalar. If \code{do_inline} is
  \code{FALSE}, this argument indicates whether
  \acronym{SQL} should be generated and passed to
  \code{FUN}, see above. If \code{do_inline} is
  \code{TRUE}, \code{simplify} determines the second
  argument passed to \code{FUN}: if \code{TRUE}, the name
  of the primary \acronym{ID} column of the database table
  is used, if \code{FALSE} the according data frame
  contained in \code{data} is passed.  If missing,
  \code{do_inline} is used as default for \code{simplify}.}
}
\value{
  \code{pkeys} yields a character vector with the
  (expected) name of the primary key column for each table.
  \code{fkeys} returns a matrix that describes the
  (expected) relations between the tables.

  \code{fkeys_valid} and \code{pkeys_valid} return
  \code{TRUE} if successful and a character vector
  describing the problems otherwise.  These functions can
  thus be used as \code{validity} argument of
  \code{setClass}.

  \code{summary} creates an S3 object that nicely prints to
  the screen (and is used by \code{show}).

  \code{length} returns the number of rows of the data
  frame in the slot defined by the first entry of
  \code{fkeys}.

  \code{head} and \code{tail} return the minimum and
  maximum available primary key, respectively, for all
  contained tables.

  \code{sort} sorts all tables by their primary keys and
  returns an object of the same class.

  \code{update} and \code{c} return an object of the same
  class than \code{object} and \code{x}, respectively.
  \code{c} runs \code{update} on all objects in \code{...}
  to yield overall unique IDs and then runs \code{rbind} on
  all tables.

  \code{split} uses \code{f} for splitting the first table
  returned by \code{pkeys} and then proceeds by accordingly
  splitting the tables that refer to it. \code{c} can be
  used to get the original object back.

  \code{by} returns the result of \code{FUN} or
  \code{INDICES} as a list or other kind of object,
  depending on \code{simplify}, if \code{do_inline} is
  \code{TRUE}.

  If \code{do_inline} is \code{FALSE}, \code{by} creates a
  novel \code{DBTABLES} object. It starts with passing
  \code{INDICES} as indexes (optionally within an
  \acronym{SQL} statement) to \code{FUN}, which should
  yield a data frame to be inserted as first table. Further
  \code{INDICES} arguments are generated using the
  information returned by \code{fkeys} to fill the object.
  Child classes might need to define a prototype with data
  frames that might be empty but already contain the column
  naming that defines the cross references.
}
\description{
  Return or check the supposed primary and foreign keys in
  a \code{\link{DBTABLES}} object, or show, traverse,
  combine, or split such objects.
}
\examples{
# example class, with 'results' referring to 'experiments'
setClass("myclass",
  contains = "DBTABLES",
  slots = c(experiments = "data.frame", results = "data.frame"))

x <- new("myclass",
  experiments = data.frame(id = 3:1, researcher = "Jane Doe"),
  results = data.frame(id = 1:5, experiment_id = c(1L, 3L, 2L, 1L, 3L),
    type = c("A", "B", "A", "B", "A"), value = runif(5)))

summary(x)
length(x) # not the number of slots
pkeys(x)
fkeys(x) # NA entries are used for table without foreign keys

# conduct some checks
stopifnot(fkeys_valid(x), pkeys_valid(x), length(x) == 3)
stopifnot(any(is.na(fkeys(x))), !all(is.na(fkeys(x))))

# originally the primary keys are not in order here
(y <- sort(x))
slot(y, "experiments")
slot(y, "results")
stopifnot(!identical(x, y))

# get first and last primary keys for each table
head(x)
tail(x)
stopifnot(head(x) == 1, tail(x) >= head(x))

# modify the primary keys
start <- 3:4 # one value per table
y <- update(x, start)
head(y)
tail(y)
stopifnot(head(y) == start, tail(y) > start)
stopifnot(fkeys_valid(y), pkeys_valid(y)) # must still be OK
(y <- update(y, NULL))
head(y)
tail(y)
stopifnot(head(y) == 1, tail(y) > 1)
stopifnot(fkeys_valid(y), pkeys_valid(y))

# split the data
(y <- split(x))
stopifnot(sapply(y, length) == 1)
stopifnot(sapply(y, fkeys_valid), sapply(y, pkeys_valid))

# combine data again
(y <- do.call(c, y))
stopifnot(length(y) == length(x), class(x) == class(y))
stopifnot(fkeys_valid(y), pkeys_valid(y))
## ids are not necessarily the same than before but still OK

# traverse the object
(y <- by(x, TRUE, function(a, b) is.data.frame(b), simplify = FALSE))
stopifnot(unlist(y), !is.null(names(y)))
(z <- by(x, 2:1, function(a, b) is.character(a), simplify = FALSE))
stopifnot(unlist(z), names(z) == rev(names(y))) # other order
(z <- by(x, 1:2, function(a, b) a, simplify = FALSE,
  do_map = c(experiments = "A", results = "B"))) # with renaming
stopifnot(unlist(z) == c("A", "B")) # new names passed as 2nd argument to FUN

# to illustrate by() in inline mode, we use a function that simply yields
# the already present slots
col_fun <- function(items, tbl, col, idx) {
  tmp <- slot(items, tbl)
  tmp[tmp[, col] \%in\% idx, , drop = FALSE]
}

(y <- by(x, slot(x, "experiments")[, "id"], col_fun, items = x,
  do_inline = TRUE, simplify = FALSE))
stopifnot(identical(y, x))

# select only a subset of the indexes
(y <- by(x, c(2L, 1L), col_fun, items = x, do_inline = TRUE,
  simplify = FALSE))
stopifnot(length(y) == 2)

# try a mapping that does not work
(y <- try(by(x, c(2L, 1L), col_fun, items = x, simplify = FALSE,
  do_inline = TRUE, do_map = c(results = "notthere")), silent = TRUE))
stopifnot(inherits(y, "try-error"))
## note that non-matching names would be silently ignored
}
\seealso{
  methods::setClass

  Other dbtables: \code{\link{DBTABLES-class}}
}
\keyword{database}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{by,DBTABLES,ANY,character,logical-method}
\alias{by,DBTABLES,ANY,character,missing-method}
\alias{by,DBTABLES,ANY,function,logical-method}
\alias{by,DBTABLES,ANY,function,missing-method}
\alias{c,DBTABLES-method}
\alias{fkeys,DBTABLES-method}
\alias{fkeys_valid,DBTABLES-method}
\alias{head,DBTABLES-method}
\alias{length,DBTABLES-method}
\alias{pkeys,DBTABLES-method}
\alias{pkeys_valid,DBTABLES-method}
\alias{show,DBTABLES-method}
\alias{sort,DBTABLES,logical-method}
\alias{sort,DBTABLES,missing-method}
\alias{split,DBTABLES,ANY,logical-method}
\alias{split,DBTABLES,ANY,missing-method}
\alias{split,DBTABLES,missing,logical-method}
\alias{split,DBTABLES,missing,missing-method}
\alias{summary,DBTABLES-method}
\alias{tail,DBTABLES-method}
\alias{update,DBTABLES-method}
\usage{
  \S4method{by}{DBTABLES,ANY,character,logical}(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) 
  \S4method{by}{DBTABLES,ANY,character,missing}(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) 
  \S4method{by}{DBTABLES,ANY,function,logical}(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) 
  \S4method{by}{DBTABLES,ANY,function,missing}(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) 

  \S4method{c}{DBTABLES}(x, ..., recursive = FALSE) 

  \S4method{fkeys}{DBTABLES}(object) 

  \S4method{fkeys_valid}{DBTABLES}(object) 

  \S4method{head}{DBTABLES}(x) 

  \S4method{length}{DBTABLES}(x) 

  \S4method{pkeys}{DBTABLES}(object) 

  \S4method{pkeys_valid}{DBTABLES}(object) 

  \S4method{show}{DBTABLES}(object) 

  \S4method{sort}{DBTABLES,logical}(x, decreasing) 
  \S4method{sort}{DBTABLES,missing}(x, decreasing) 

  \S4method{split}{DBTABLES,ANY,logical}(x, f, drop) 
  \S4method{split}{DBTABLES,ANY,missing}(x, f, drop) 
  \S4method{split}{DBTABLES,missing,logical}(x, f, drop) 
  \S4method{split}{DBTABLES,missing,missing}(x, f, drop) 

  \S4method{summary}{DBTABLES}(object) 

  \S4method{tail}{DBTABLES}(x) 

  \S4method{update}{DBTABLES}(object, start, drop = TRUE) 
}
%% END INSERTION BY repair_S4_docu.rb
