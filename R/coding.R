
################################################################################


#' Modified switch function
#'
#' An altered \code{switch} statement for stricter flow control.
#'
#' @param EXPR A character or numeric scalar based on which a decision is made.
#' @param ... Additional arguments from which to select an alternative.
#' @return Selected value from \code{\dots}.
#' @details If \code{EXPR} is a character scalar, the behaviour is like
#'   the one of \code{switch} with the exception that unmatched values within
#'   \code{\dots} cause an error. If \code{EXPR} is of mode \sQuote{numeric},
#'   the behaviour is like \code{switch} but counting starts at 0 and a value
#'   larger than the number of elements within \code{\dots} selects the last
#'   element. It is an error if \code{EXPR} is negative or if \code{\dots}
#'   contains no arguments at all.
#' @export
#' @seealso base::switch
#' @family coding-functions
#' @keywords utilities
#' @examples
#'
#' # 'numeric' method
#' (x <- case(0, "a", "b", "c"))
#' stopifnot(identical(x, "a"))
#' (x <- case(99, "a", "b", "c"))
#' stopifnot(identical(x, "c"))
#'
#' # 'character' method
#' (x <- case("b", a = "x", b = "y", c = "z"))
#' stopifnot(identical(x, "y"))
#' (x <- try(case("d", a = "x", b = "y", c = "z"), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#'
case <- function(EXPR, ...) UseMethod("case")

#' @rdname case
#' @method case numeric
#' @export
#'
case.numeric <- function(EXPR, ...) {
  stopifnot(EXPR >= 0L, nargs() > 1L)
  switch(EXPR = pmin(EXPR, nargs() - 2L) + 1L, ...)
}

#' @rdname case
#' @method case character
#' @export
#'
case.character <- function(EXPR, ...) {
  switch(EXPR = EXPR, ..., stop("unmatched 'EXPR' value"))
}


################################################################################


#' Convert warnings to errors
#'
#' Raise an error if a warning occurs. Useful for making certain tests more
#' strict. It is a bit easier to use than changing the \sQuote{warn} entry of
#' \code{options} from the \pkg{base} package (because the entry would usually
#' need to be set back).
#'
#' @param expr \R expression to evaluate.
#' @param msg Character vector to be used as error message. If empty or
#'   \code{NULL}, the \code{conditionMessage} of the issued warning is used.
#' @param ... Optional further arguments to \code{tryCatch}.
#' @param domain Passed to \code{stop} (if a warning occurs).
#' @return The result of \code{expr} (if no error occurs).
#' @export
#' @seealso base::tryCatch base::stop base::options
#' @family coding-functions
#' @keywords utilities
#' @examples
#' (x <- try(must(as.numeric(c("1", "2", "3"))), silent = TRUE))
#' stopifnot(identical(x, c(1, 2, 3)))
#' (x <- try(must(as.numeric(c("1", "x", "3"))), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#'
must <- function(expr, msg = NULL, ..., domain = NULL) {
  # For some reason, using stop() directly results in errors that cannot be
  # catched with tryCatch() any more.
  tryCatch(expr = expr, warning = function(w) stop(if (length(msg))
    msg
  else
    conditionMessage(w), call. = FALSE, domain = domain), ...)
}


################################################################################


#' Assert a length
#'
#' Raise an error if one to several given \R objects do not have the specified
#' length. This is mainly used to easily generate meaningful error messages
#' related to function arguments.
#'
#' @param x \R object to test.
#' @param ... Any \R objects to test.
#' @param .wanted Integer scalar giving the desired length. Note that this can
#'   \strong{not} be a scalar with \sQuote{double} as \code{storage.mode}.
#' @param .msg Error message passed to \code{sprintf} with the name of \code{x}
#'   and the value of \code{wanted} as the two additional arguments.
#' @param .domain Passed to \code{stop} from the \pkg{base} package as argument
#'   \sQuote{domain}.
#' @return If successful, \code{L} returns \code{x}, but an error message is
#'   raised if \code{length(x)} is not identical to \code{wanted}. \code{LL}
#'   yields the names of the arguments contained in \code{\dots}, returned
#'   invisibly, if successful. Otherwise an error is raised.
#' @seealso base::stop
#' @export
#' @family coding-functions
#' @keywords utilities
#' @examples
#' (x <- L(letters, 26L))
#' stopifnot(identical(x, letters))
#' (x <- try(L(letters, 25L), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#' (x <- LL(letters, LETTERS, .wanted = 26L))
#' stopifnot(x == c("letters", "LETTERS"))
#'
L <- function(x, .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  if (identical(length(x), .wanted))
    return(x)
  stop(sprintf(.msg, deparse(match.call()$x), .wanted), call. = FALSE,
    domain = .domain)
}

#' @rdname L
#' @export
#'
LL <- function(..., .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  arg.names <- as.character(match.call())[-1L][seq_along(items <- list(...))]
  invisible(mapply(function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf(.msg, name, .wanted), call. = FALSE, domain = .domain)
    name
  }, items, arg.names, SIMPLIFY = TRUE, USE.NAMES = FALSE))
}


################################################################################


#' Nicer message listings
#'
#' Create some kind of listing, used, e.g., in (error) messages or warnings.
#'
#' @inheritParams pack_desc
#' @param x For the default method, an object convertible via \code{unclass} to
#'   one of the object classes that have explicit methods. For the
#'   character-vector method, in the default style mode, its \sQuote{names}
#'   attribute is used as the first column of the resulting listing; if it is
#'   \code{NULL} or if \code{force.numbers} is \code{TRUE}, numbers are
#'   inserted. The \sQuote{double} method is controlled by the \sQuote{digits}
#'   entry of \code{options} from the \pkg{base} package.
#' @param header \code{NULL} or character vector. Prepended to the result.
#' @param footer \code{NULL} or character vector. Appended to the result.
#' @param prepend Logical, numeric or character scalar. The two main uses are:
#'   \describe{
#'   \item{Default mode:}{The value is prepended to each line except
#'     \code{header} and \code{footer}. If numeric, the number of spaces.
#'     \code{TRUE} causes tabs to be used, \code{FALSE} turns prepending off. If
#'     in \sQuote{character} mode, used directly.}
#'   \item{If \code{style} is \sQuote{sentence}:}{ In that case, a logical
#'     scalar decides about whether names are prepended before joining the
#'     vector elements. A character scalar is used as template for
#'     \code{sprintf}, which gets \code{names(x)} passed as second and \code{x}
#'     as third argument. (This order can be inversed, see next argument.)}
#'   }
#' @param style Character scalar. The main options are:
#'   \describe{
#'   \item{\sQuote{table} or \sQuote{list}:}{Passed to \code{formatDL}.}
#'   \item{\sQuote{sentence}:}{A comma-separated list is created from \code{x},
#'   the last separator according to \code{last.sep}.}
#'   \item{\sQuote{m4} or \sQuote{M4}}{acronym{GNU} \command{m4} macro
#'   definitions using double or single quoting, respectively, for the
#'   expansions are created. A warning is issued if the macro strings are
#'   invalid, which is always the case of \code{x} has no names; \code{prepend}
#'   is ignored.}
#'   \item{Otherwise:}{A template for \code{sprintf} is assumed taking two
#'   additional arguments, first \code{names(x)} and then \code{x}.}
#'   }
#'   Note that names and values of \code{x} are exchanged beforehand if
#'   \code{style} is run through \code{I} from the \pkg{base} package.
#' @param collapse Character scalar used to join the resulting vector elements.
#'   It is by default also applied for joining \code{header} and \code{footer}
#'   footer with them (if provided). This can be turned off using hf.collapse.
#'   By default this is an empty string for \sQuote{sentence} style, the newline
#'   character otherwise.
#' @param force.numbers Logical scalar. Always use numbers instead of the
#'   \sQuote{names} attribute?
#' @param last.sep Character scalar indicating what should be used as last
#'   separator if \code{style} is \sQuote{sentence}. \sQuote{both} means
#'   \sQuote{and} and comma, \sQuote{two} means \sQuote{or} and comma.
#' @param hf.collapse Character scalar or empty. If distinct from
#'   \code{collapse}, used for separately for joining \code{header} and
#'   \code{footer} (if provided).
#' @param ... Optional other arguments passed to \code{formatDL}.
#' @return Character scalar.
#' @export
#' @seealso base::message base::warning base::stop base::formatDL
#' @family coding-functions
#' @keywords utilities
#' @examples
#'
#' # default style
#' x <- structure(letters[1:5], names = LETTERS[1:5])
#' (y <- listing(x, "Five letters:", "...end here", 1))
#' stopifnot(length(y) == 1, y ==
#'   "Five letters:\n A: a\n B: b\n C: c\n D: d\n E: e\n...end here")
#'
#' # 'sentence' style
#' (y <- listing(letters[1:3], style = "sentence", last.sep = "both"))
#' stopifnot(y == "a, b, and c", length(y) == 1)
#' (y <- listing(letters[1:3], style = I("sentence"), last.sep = "both"))
#' stopifnot(y == "1, 2, and 3", length(y) == 1)
#' (y <- listing(letters[1:3], style = "sentence", prepend = TRUE))
#' stopifnot(y == "1: a, 2: b and 3: c", length(y) == 1)
#' (y <- listing(letters[1:3], style = I("sentence"), prepend = TRUE))
#' stopifnot(y == "a: 1, b: 2 and c: 3", length(y) == 1)
#' (y <- listing(letters[1:3], style = "sentence", prepend = "%s=>%s"))
#' stopifnot(y == "1=>a, 2=>b and 3=>c", length(y) == 1)
#' (y <- listing(letters[1:3], style = "sentence", last.sep = "two"))
#' stopifnot(y == "a, b, or c", length(y) == 1)
#'
#' # with explicit sprintf template
#' (y <- listing(x, style = "%s, %s", collapse = "; ", prepend = "!"))
#' stopifnot(y == "!A, a; !B, b; !C, c; !D, d; !E, e", length(y) == 1)
#' (y <- listing(x, style = I("%s, %s"), collapse = "; ", prepend = "!"))
#' stopifnot(y == "!a, A; !b, B; !c, C; !d, D; !e, E", length(y) == 1)
#'
#' # create m4 macro definitions
#' (y <- listing(x, style = "m4"))
#' stopifnot(grepl("^(define\\([^)]+\\)dnl\n?)+$", y), length(y) == 1)
#'
#' # other 'x' arguments
#' stopifnot(listing(x) == listing(as.list(x)))
#' stopifnot(listing(pi) == "1: 3.141593") # controlled by getOption("digits")
#'
listing <- function(x, ...) UseMethod("listing")

#' @rdname listing
#' @method listing double
#' @export
#'
listing.double <- function(x, ...) {
  x <- signif(x, getOption("digits"))
  mode(x) <- "character"
  listing.character(x, ...)
}

#' @rdname listing
#' @method listing factor
#' @export
#'
listing.factor <- function(x, ...) {
  y <- as.character(x)
  names(y) <- names(x)
  listing.character(y, ...)
}

#' @rdname listing
#' @method listing default
#' @export
#'
listing.default <- function(x, ...) {
  listing(c(unclass(x)), ...)
}

#' @rdname listing
#' @method listing list
#' @export
#'
listing.list <- function(x, ...) {
  listing(unlist(x), ...)
}

#' @rdname listing
#' @method listing character
#' @export
#'
listing.character <- function(x, header = NULL, footer = NULL, prepend = FALSE,
    style = "list", collapse = if (style == "sentence")
      ""
    else
      "\n", force.numbers = FALSE,
    last.sep = c("and", "both", "comma", "or", "two"), hf.collapse = collapse,
    ...) {

  spaces <- function(x) {
    if (is.character(x))
      x
    else if (is.numeric(x))
      sprintf(sprintf("%%%is", x), "")
    else if (x)
      "\t"
    else
      ""
  }

  do_prepend <- function(x, prepend) paste0(spaces(L(prepend)), x)

  sentence <- function(x, last, prepend) {
    get_last_sep <- function(last) case(last, and = " and ", comma = ", ",
      both = ", and ", or = " or ", two = ", or ")
    if (is.character(prepend))
      x <- sprintf(prepend, names(x), x)
    else if (prepend)
      x <- paste(names(x), x, sep = ": ")
    case(n <- length(x),
      stop("empty 'x' argument in 'sentence' mode"),
      x,
      paste0(x, collapse = get_last_sep(last)),
      paste(paste0(x[-n], collapse = ", "), x[n], sep = get_last_sep(last))
    )
  }

  to_m4 <- function(x, single) {
    is_macro <- function(x) grepl("^[A-Za-z_]\\w*$", x, perl = TRUE)
    do_quote <- function(x, single) {
      x <- chartr("`", "'", x)
      if (single)
        sprintf("`%s'", gsub("'", "''`", x, fixed = TRUE))
      else
        sprintf("``%s''", gsub("'", "'''``", x, fixed = TRUE))
    }
    if (any(bad <- !is_macro(y <- names(x))))
      warning(sprintf("not a valid m4 macro string: '%s'", y[bad][1L]))
    sprintf("define(%s, %s)dnl", do_quote(y, TRUE), do_quote(x, single))
  }

  LL(style, collapse, force.numbers, hf.collapse)
  if ((is.null(names(x)) && style != "insert") || force.numbers)
    names(x) <- seq_along(x)
  if (inherits(style, "AsIs"))
    x <- structure(names(x), names = x)
  switch(style,
    table =,
    list = x <- do_prepend(formatDL(x = x, style = style, ...), prepend),
    sentence = x <- sentence(x, match.arg(last.sep), prepend),
    m4 = x <- to_m4(x, FALSE),
    M4 = x <- to_m4(x, TRUE),
    x <- do_prepend(sprintf(style, names(x), x), prepend)
  )
  if (identical(collapse, hf.collapse))
    paste0(c(header, x, footer), collapse = collapse)
  else
    paste0(c(header, paste0(x, collapse = collapse), footer),
      collapse = hf.collapse)
}


################################################################################


#' Flatten a list or collect information from it
#'
#' Methods for making an object \sQuote{flat}, such as creating a non-nested
#' list from a list, and methods for collecting information from list-like
#' objects into a matrix or data frame.
#'
#' @param object Usually a list. The default method just returns \code{object}
#'   if it is atomic but raises an error otherwise.
#' @param use.names Logical scalar passed to \code{unlist} from the \pkg{base}
#'   package.
#' @param x List.
#' @param what Character scalar indicating how to collect information.
#'   \describe{
#'   \item{counts}{For all non-list elements of \code{x}, count their
#'   occurrences.}
#'   \item{occurrences}{Like \sQuote{counts}, but only indicate presence or
#'   absence.}
#'   \item{values}{Simplify all direct elements of \code{x}, irrespective of
#'   whether or not they are lists, for including them as rows in a data frame.
#'   Their names determine the columns. See \code{keep.unnamed} for the action
#'   in the case of missing names.}
#'   \item{elements}{Like \sQuote{elements}, but collect only the non-list
#'   elements of \code{x}, i.e. flatten \code{x} in the first step.}
#'   \item{datasets}{Convert all elements to data frames or matrices, then merge
#'   them using rows and column names. In case of conflict, the last ones win.
#'   Here, the behaviour of other arguments is special if all elements of
#'   \code{x} are atomic. See below.}
#'   }
#' @param min.cov Numeric scalar indicating the mimimal coverage required in the
#'   resulting presence-absence matrix. Columns with a fewer number of non-zero
#'   entries are removed.
#' @param keep.unnamed Logical scalar indicating whether names should be
#'   inserted for elements of \code{x} that miss them. If \code{NA}, they are
#'   skipped, but with a warning; if \code{FALSE}, they are skipped silently.
#'   This has only an effect in conjunction with the last three values of
#'   \code{what}. If \sQuote{datasets} are chosen, it usually has only an
#'   effect if all elements of \code{x} are atomic.
#' @param dataframe Logical scalar indicating whether a data frame should be
#'   produced instead of a matrix
#' @param optional See \code{as.data.frame} from the \pkg{base} package.
#' @param stringsAsFactors See \code{as.data.frame} from the \pkg{base} package.
#' @param ... Optional arguments passed to and from other methods (if requested
#'   to \code{as.data.frame}).
#' @export
#' @return The list method of \code{flatten} returns a non-nested list. The
#'   \code{collect} methods yield a data frame or a matrix.
#' @family coding-functions
#' @details The list method of \code{flatten} is based on
#'   \url{http://stackoverflow.com/questions/8139677/} with some slight
#'   improvements.
#' @seealso base::unlist base::as.data.frame
#' @keywords manip
#' @examples
#'
#' ## flatten()
#' x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
#'   e = list(pi))
#' (y <- flatten(x)) # sublists removed, non-list elements kept
#' stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
#' # atomic objects are not modified by default
#' stopifnot(identical(letters, flatten(letters)))
#'
#' ## collect()
#' x <- list(X = list(A = 1:3, B = 7L, C = list('c1', 1:3)),
#'   Y = list(A = 1:3, 11, B = -1L, D = "?"))
#'
#' ## collect values into a data frame or matrix
#' (got <- collect(x, "values", dataframe = TRUE))
#' stopifnot(LETTERS[1:4] == colnames(got))
#' stopifnot(names(x) == rownames(got))
#' stopifnot(is.list(got$A), is.integer(got$B), is.list(got$C),
#'   is.factor(got$D))
#' stopifnot(!is.na(got$A), !is.na(got$B), any(is.na(got$C)), any(is.na(got$D)))
#' # include the unnamed ones
#' got <- collect(x, "values", dataframe = TRUE, keep.unnamed = TRUE)
#' stopifnot(dim(got) == c(2, 5))
#' # simplify to matrix
#' (got <- collect(x, "values", dataframe = FALSE))
#' stopifnot(is.matrix(got), mode(got) == "list")
#'
#' ## collect elements into a data frame or matrix
#' (got <- collect(x, "elements", dataframe = TRUE))
#' stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
#'   is.data.frame(got))
#' (got <- collect(x, "elements", dataframe = FALSE))
#' stopifnot(dim(got) == c(2, 9), colnames(x) == rownames(got),
#'   !is.data.frame(got))
#'
#' ## count or just note occurrences
#' (got <- collect(x, "counts", dataframe = FALSE))
#' stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
#'   setequal(colnames(got), unlist(x)), any(got > 1))
#' (got <- collect(x, "occurrences", dataframe = FALSE))
#' stopifnot(dim(got) == c(2, 8), rownames(got) == names(x),
#'   setequal(colnames(got), unlist(x)), !any(got > 1))
#'
#' ## convert to data frames and insert everything in a single one
#' (got <- collect(x, "datasets", optional = FALSE, dataframe = TRUE))
#' stopifnot(dim(got) == c(3, 6), is.data.frame(got))
#'
#' ## a more useful application is to merge matrices
#' m1 <- matrix(1:4, ncol = 2, dimnames = list(c("A", "B"), c("x", "y")))
#' m2 <- matrix(1:4, ncol = 2, dimnames = list(c("C", "B"), c("x", "z")))
#' (got <- collect(list(m1, m2), "datasets"))
#' # values missing in some matrix yield NA
#' stopifnot(dim(got) == c(3, 3), any(is.na(got)))
#'
flatten <- function(object, ...) UseMethod("flatten")

#' @rdname flatten
#' @method flatten default
#' @export
#'
flatten.default <- function(object, ...) {
  if (is.atomic(object))
    return(object)
  stop("need atomic 'object' (or specific 'flatten' method)")
}

#' @rdname flatten
#' @method flatten list
#' @export
#'
flatten.list <- function(object, use.names = TRUE, ...) {
  while (any(is.a.list <- vapply(object, is.list, NA))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, FALSE, use.names)
  }
  object
}

#' @rdname flatten
#' @export
#'
collect <- function(x, what, ...) UseMethod("collect")

#' @rdname flatten
#' @method collect list
#' @export
#'
collect.list <- function(x,
    what = c("counts", "occurrences", "values", "elements", "datasets"),
    min.cov = 1L, keep.unnamed = FALSE, dataframe = FALSE, optional = TRUE,
    stringsAsFactors = default.stringsAsFactors(), ...) {

  # collecting 'counts' or 'occurrences'
  note_in_matrix <- function(x, count, min.cov, dataframe, optional,
      stringsAsFactors, ...) {
    x <- lapply(lapply(x, unlist), as.character)
    n <- sort.int(unique.default(unlist(x, FALSE, FALSE)))
    result <- matrix(0L, length(x), length(n), FALSE, list(names(x), n))
    if (count)
      for (i in seq_along(x)) {
        value <- table(x[[i]], useNA = "ifany")
        result[i, names(value)] <- unclass(value)
      }
    else
      for (i in seq_along(x))
        result[i, x[[i]]] <- 1L
    if (min.cov > 0L)
      result <- result[, colSums(result) >= min.cov, drop = FALSE]
    if (dataframe)
      as.data.frame(x = result, optional = optional,
        stringsAsFactors = stringsAsFactors, ...)
    else
      result
  }

  # collecting 'values' or 'elements'
  insert_into_matrix <- function(x, flat, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...) {
    oneify <- function(x) {
      if (is.atomic(x))
        return(x)
      size <- vapply(x, length, 0L)
      x[!size] <- NA
      x[size > 1L] <- lapply(x[size > 1L], list)
      x
    }
    keep_validly_named_only <- function(x) {
      if (is.null(names(x)))
        NULL
      else
        x[nzchar(names(x))]
    }
    keep_validly_named_only_but_complain <- function(x) {
      if (is.null(names(x))) {
        warning("removing unnamed vector")
        NULL
      } else if (!all(ok <- nzchar(names(x)))) {
        warning("removing elements with empty names")
        x[ok]
      } else
        x
    }
    enforce_names <- function(x) {
      names(x) <- if (is.null(n <- names(x)))
          seq_along(x)
        else
          ifelse(nzchar(n), n, seq_along(x))
      x
    }
    if (flat)
      x <- lapply(x, unlist)
    else
      x <- lapply(x, oneify)
    if (keep.unnamed)
      x <- lapply(x, enforce_names)
    else if (verbose)
      x <- lapply(x, keep_validly_named_only_but_complain)
    else
      x <- lapply(x, keep_validly_named_only)
    n <- unique.default(unlist(lapply(x, names), FALSE))
    result <- matrix(NA, length(x), length(n), FALSE, list(names(x), n))
    result <- as.data.frame(x = result, stringsAsFactors = FALSE,
      optional = TRUE, ...)
    for (i in seq_along(x))
      result[i, names(x[[i]])] <- x[[i]]
    if (dataframe && stringsAsFactors)
      for (i in which(vapply(result, is.character, NA)))
        result[, i] <- as.factor(result[, i])
    if (!optional)
      names(result) <- make.names(names(result))
    if (dataframe)
      result
    else
      as.matrix(result)
  }

  # collecting 'datasets'
  collect_matrices <- function(x, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...) {
    keep_validly_named_only <- function(x) {
      if (is.null(colnames(x)) || is.null(rownames(x)))
        NULL
      else
        x[nzchar(rownames(x)), nzchar(colnames(x)), drop = FALSE]
    }
    keep_validly_named_only_but_complain <- function(x) {
      if (is.null(colnames(x)) || is.null(rownames(x))) {
        warning("removing unnamed matrix or data frame")
        NULL
      } else {
        row.ok <- nzchar(rownames(x))
        col.ok <- nzchar(colnames(x))
        if (!all(row.ok) || !all(col.ok)) {
          warning("removing rows and/or columns with empty names")
          x[row.ok, col.ok, drop = FALSE]
        } else
          x
      }
    }
    enforce_names <- function(x) {
      enforce <- function(x, n) if (is.null(x))
          seq_len(n)
        else
          ifelse(nzchar(x), x, seq_len(n))
      rownames(x) <- enforce(rownames(x), nrow(x))
      colnames(x) <- enforce(colnames(x), ncol(x))
      x
    }
    if (all.atomic <- all(vapply(x, is.atomic, NA))) {
      x <- lapply(x, as.matrix)
      if (keep.unnamed)
        x <- lapply(x, enforce_names)
      else if (verbose)
        x <- lapply(x, keep_validly_named_only_but_complain)
      else
        x <- lapply(x, keep_validly_named_only)
    } else
      x <- lapply(x, data.frame, stringsAsFactors = FALSE,
        check.names = !optional)
    rn <- sort.int(unique.default(unlist(lapply(x, rownames))))
    cn <- sort.int(unique.default(unlist(lapply(x, colnames))))
    result <- matrix(NA, length(rn), length(cn), FALSE, list(rn, cn))
    if (!all.atomic)
      result <- as.data.frame(result, stringsAsFactors = FALSE,
        optional = optional)
    for (mat in x)
      result[rownames(mat), colnames(mat)] <- mat
    if (dataframe) {
      if (all.atomic)
        result <- as.data.frame(result)
      if (stringsAsFactors)
        for (i in which(vapply(result, is.character, NA)))
          result[, i] <- as.factor(result[, i])
    } else if (!all.atomic)
      result <- as.matrix(result)
    if (!optional)
      colnames(result) <- make.names(colnames(result))
    result
  }

  LL(min.cov, dataframe, optional, stringsAsFactors, keep.unnamed)
  if (verbose <- is.na(keep.unnamed))
    keep.unnamed <- FALSE
  case(match.arg(what),
    counts = note_in_matrix(x, TRUE, min.cov, dataframe, optional,
      stringsAsFactors, ...),
    occurrences = note_in_matrix(x, FALSE, min.cov, dataframe, optional,
      stringsAsFactors, ...),
    elements = insert_into_matrix(x, TRUE, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...),
    values = insert_into_matrix(x, FALSE, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...),
    datasets = collect_matrices(x, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...)
  )
}


################################################################################
