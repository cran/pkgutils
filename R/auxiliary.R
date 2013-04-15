


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
#' @family auxiliary-functions
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
#' @family auxiliary-functions
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
#' Raise an error if a given \R object does not have the specified length. This
#' is mainly used to easily generate meaningful error messages related to
#' function arguments.
#'
#' @param x \R object to test.
#' @param .wanted Integer scalar giving the desired length. Note that this can
#'   \strong{not} be a scalar with \sQuote{double} as \code{storage.mode}.
#' @param .msg Error message passed to \code{sprintf} with the name of \code{x}
#'   and the value of \code{wanted} as the two additional arguments.
#' @param .domain Passed to \code{stop} from the \pkg{base} package as argument
#'   \sQuote{domain}.
#' @return If successful, \code{x}, but an error message is raised if
#'   \code{length(x)} is not identical to \code{wanted}.
#' @seealso base::stop
#' @export
#' @family auxiliary-functions
#' @keywords utilities
#' @examples
#' (x <- L(letters, 26L))
#' stopifnot(identical(x, letters))
#' (x <- try(L(letters, 25L), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#'
L <- function(x, .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  if (identical(length(x), .wanted))
    return(x)
  stop(sprintf(.msg, as.character(match.call()[2L]), .wanted), call. = FALSE,
    domain = .domain)
}


################################################################################


#' Assert lengths
#'
#' Raise an error if one of several \R objects does not have the specified
#' length. This is mainly used to easily generate meaningful error messages
#' related to function arguments.
#'
#' @inheritParams L
#' @param ... Any \R objects to test.
#' @return The names of the arguments contained in \code{\dots}, returned
#'   invisibly, if successful. Otherwise an error is raised.
#' @seealso base::stop
#' @export
#' @family auxiliary-functions
#' @keywords utilities
#' @examples
#' (x <- LL(letters, LETTERS, .wanted = 26L))
#' stopifnot(x == c("letters", "LETTERS"))
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
#'   It is by default also applied jor joining \code{header} and \code{footer}
#'   footer with them (if provided). This can be turned off using hf.collapse.
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
#' @family auxiliary-functions
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
    last.sep = c("and", "both", "comma", "or", "two"),
    hf.collapse = collapse, ...) {

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

  do_prepend <- function(x, prepend) paste(spaces(prepend), x, sep = "")

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
      paste(x, collapse = get_last_sep(last)),
      paste(paste(x[-n], collapse = ", "), x[n], sep = get_last_sep(last))
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

  LL(style, collapse, force.numbers, prepend)
  if (is.null(names(x)) || force.numbers)
    names(x) <- seq_along(x)
  if (inherits(style, "AsIs"))
    x <- structure(.Data = names(x), .Names = x)
  x <- switch(style,
    table =,
    list = do_prepend(formatDL(x = x, style = style, ...), prepend),
    sentence = sentence(x, match.arg(last.sep), prepend),
    m4 = to_m4(x, FALSE),
    M4 = to_m4(x, TRUE),
    do_prepend(sprintf(style, names(x), x), prepend)
  )
  if (identical(collapse, hf.collapse))
    paste(c(header, x, footer), collapse = collapse)
  else
    paste(c(header, paste(x, collapse = collapse), footer),
      collapse = L(hf.collapse))
}


################################################################################


#' Flatten a list
#'
#' Create a non-nested list.
#'
#' @param object Usually a list. The default method just returns \code{object}.
#' @inheritParams pack_desc
#' @export
#' @return A list.
#' @family auxiliary-functions
#' @note The method is based on 
#'   \url{http://stackoverflow.com/questions/8139677/} with some slight
#'   improvements.
#' @keywords manip
#' @examples
#' x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3],
#'   e = list(pi))
#' (y <- flatten(x))
#' stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
#' stopifnot(identical(letters, flatten(letters)))
#'
flatten <- function(object, ...) UseMethod("flatten")

#' @rdname flatten
#' @method flatten default
#' @export
#'
flatten.default <- function(object, ...) object

#' @rdname flatten
#' @method flatten list
#' @export
#'
flatten.list <- function(object, ...) {
  while (any(is.a.list <- vapply(object, is.list, logical(1L)))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, recursive = FALSE)
  }
  object
}


################################################################################


#' Get or set logfile
#'
#' Get or set the name of the logfile used by \pkg{pkgutils}.
#'
#' @param x Character scalar for setting the logfile, or \code{NULL} for
#'   getting the current value.
#' @export
#' @return Character scalar with the name of the current logfile. Use an empty
#'   string to turn logging off.
#' @details Functions such as \code{\link{check_keywords}} print detected
#'   problems, if any, using \code{message}. These character vectors can also
#'   be safed by appending to a logfile.
#' @keywords IO
#' @family auxiliary-functions
#' @examples
#' old <- logfile()
#' new <- tempfile()
#' logfile(new)
#' stopifnot(new == logfile())
#' logfile(old)
#' stopifnot(old == logfile())
#'
logfile <- function(x) UseMethod("logfile")

#' @rdname logfile
#' @method logfile NULL
#' @export
#'
logfile.NULL <- function(x) {
  PKGUTILS_OPTIONS$logfile
}

#' @rdname logfile
#' @method logfile character
#' @export
#'
logfile.character <- function(x) {
  old <- PKGUTILS_OPTIONS$logfile
  PKGUTILS_OPTIONS$logfile <- L(x[complete.cases(x)])
  if (nzchar(x))
    tryCatch(cat(sprintf("\nLOGFILE RESET AT %s\n", date()), file = x,
      append = TRUE), error = function(e) {
        PKGUTILS_OPTIONS$logfile <- old
        stop(e)
      })
  invisible(x)
}


################################################################################


#' Map files
#'
#' Read lines from a file, modify the lines using a given function, and write
#' the lines back to the input file unless the result of applying the function
#' is identical to the lines read.
#'
#' @param x Character vector of input (and potentially output) file names.
#' @param mapfun Mapping function, receives character vector with the lines per
#'   file as first argument, with the name of the file added as attribute with
#'   the name given using \code{.attr}.
#' @param ... Optional additional arguments passed to \code{fun}.
#' @param .attr Character scalar. See description to \code{mapfun}.
#' @param .encoding Passed to \code{readLines} as \sQuote{encoding} argument.
#' @param .sep \code{NULL} or character scalar. If empty, ignored. Otherwise
#'   used as output line separator, causing outfiles to always be written
#'   unless \code{mapfun} returns \code{NULL}. Can be used to just change line
#'   breaks if \code{mapfun} is \code{identity}.
#' @details If \code{mapfun} returns \code{NULL}, it is ignored. Otherwise
#'   is it an error if \code{mapfun} does not return a character vector. If
#'   this vector is identical to the lines read from the file, it is not
#'   printed to this file unless \code{sep} is non-empty. Otherwise the file
#'   is attempted to be overwritten with the result of \code{mapfun}.
#' @return Logical vector using \code{x} as names, with \code{TRUE} indicating
#'   a successfully modified file, \code{FALSE} a file that yielded no errors
#'   but needed not to be modified, and \code{NA} a filename that caused an
#'   error. An attribute \sQuote{errors} is provided, containing a character
#'   vector with error messages (empty strings if no error occurred).
#' @seealso base::readLines base::writeLines base::identity
#' @family auxiliary-functions
#' @export
#' @keywords IO
#' @examples
#' tmpfile <- tempfile()
#' write(letters, file = tmpfile)
#' (x <- map_files(tmpfile, identity))
#' stopifnot(!x)
#' # now enforce other output line separator
#' (x <- map_files(tmpfile, identity, .sep = "\n"))
#' stopifnot(x)
#' (x <- map_files(tmpfile, toupper))
#' stopifnot(x)
#' x <- readLines(tmpfile)
#' stopifnot(x == LETTERS)
#' (x <- map_files(tmpfile, as.null))
#' stopifnot(!x)
#'
map_files <- function(x, ...) UseMethod("map_files")

#' @method map_files character
#' @rdname map_files
#' @export
#'
map_files.character <- function(x, mapfun, ..., .attr = ".filename",
    .encoding = "", .sep = NULL) {
  doit <- function(filename) tryCatch({
    add_attr <- function(x) {
      attr(x, .attr) <- filename
      x
    }
    connection <- file(description = filename, encoding = .encoding)
    x <- readLines(con = connection)
    close(connection)
    if (is.null(y <- mapfun(add_attr(x), ...))) # shortcut
      return(list(FALSE, ""))
    if (optional.output) {
      attributes(y) <- NULL
      if (identical(x, y))
        return(list(FALSE, ""))
    }
    if (!is.character(y))
      stop("applying 'matchfun' did not yield a character vector")
    writeLines(text = y, con = filename, sep = sep)
    list(TRUE, "")
  }, error = function(e) list(NA, conditionMessage(e)))
  case(length(.sep),
    {
      optional.output <- TRUE
      if (grepl("windows", .Platform$OS.type, ignore.case = TRUE, perl = TRUE))
        sep <- "\r\n"
      else
        sep <- "\n"
    },
    {
      optional.output <- FALSE
      sep <- .sep
    },
    stop("'.sep' must be of length 0 or 1")
  )
  mapfun <- match.fun(mapfun)
  if (!length(x))
    return(structure(logical(), .Names = character(), errors = character()))
  result <- do.call(rbind, lapply(x, doit))
  structure(unlist(result[, 1L]), .Names = x, errors = unlist(result[, 2L]))
}


################################################################################


#' Run a Ruby script
#'
#' Run Ruby with an externally provided Ruby script or with code provided at
#' the command line with \sQuote{-e}.
#'
#' @param x Character vector containing the name of a script and optionally,
#'   after that name, the script's arguments. If a numeric vector, a required
#'   minimum Ruby version. A command is then constructed that only results if
#'   this version requirement is met. If \code{NULL}, the path to the Ruby
#'   executable is returned, or an empty string if this is not found.
#' @param args Character vector with arguments passed to Ruby before the
#'   content of \code{x}. \sQuote{--} is appended automatically. Note that any
#'   \sQuote{-e} argument would cause a character vector \code{x} to be
#'   ignored, and that otherwise an empty \code{x} character vector would cause
#'   the Ruby process to hang (wait for input that will not arrive).
#' @param ruby Character scalar containing the name of the Ruby executable. It
#'   is an error if this file is not found using \code{Sys.which}.
#' @param ... Optional arguments (except \sQuote{command}) passed to
#'   \code{system} from the \pkg{base} package.
#' @return Unless \code{x} is \code{NULL}, the result of a call to
#'   \code{system}. This is an integer scalar unless \code{\dots} dictates
#'   otherwise.
#' @keywords interface
#' @export
#' @seealso base::system base::Sys.which
#' @family auxiliary-functions
#' @examples
#' if (nzchar(run_ruby(NULL))) {
#'   # run a dummy Ruby command that does nothing
#'   (x <- run_ruby(x = character(), args = "-e'nil'"))
#'   stopifnot(identical(x, 0L))
#' } else {
#'   warning("cannot find 'ruby'")
#' }
#'
run_ruby <- function(x, ...) UseMethod("run_ruby")

#' @rdname run_ruby
#' @method run_ruby NULL
#' @export
#'
run_ruby.NULL <- function(x, ruby = "ruby", ...) {
  unname(Sys.which(L(ruby)))
}

#' @rdname run_ruby
#' @method run_ruby numeric
#' @export
#'
run_ruby.numeric <- function(x, args = "-w", ruby = "ruby", ...) {
  y <- "-e'raise \"need Ruby %.1f.0 or higher\" if RUBY_VERSION.to_f < %.1f'"
  args <- c(sprintf(y, x, x), args)
  run_ruby(x = character(), args = args, ruby = ruby, ...)
}

#' @rdname run_ruby
#' @method run_ruby character
#' @export
#'
run_ruby.character <- function(x, args = "-w", ruby = "ruby", ...) {
  if (!nzchar(ruby <- run_ruby(x = NULL, ruby = ruby)))
    stop(sprintf("cannot find executable '%s'", ruby))
  command <- paste(c(ruby, setdiff(args, "--"), "--", x), collapse = " ")
  do.call(system, list(command = command, ...))
}


################################################################################


#' Run a Ruby script provided by the package
#'
#' Run Ruby with one of the Ruby scripts that come with the \pkg{pkgutils}
#' package, and a selection of input files.
#'
#' @inheritParams repair_S4_docu
#' @param script Character scalar indicating the name of the script to use
#'   (without directory name).
#' @param ... Optional arguments passed to \code{\link{run_ruby}}.
#' @return Result of a call to \code{\link{run_ruby}}.
#' @keywords internal
#'
run_pkgutils_ruby <- function(x, ...) UseMethod("run_pkgutils_ruby")

#' @rdname run_pkgutils_ruby
#' @method run_pkgutils_ruby character
#'
run_pkgutils_ruby.character <- function(x, script, ignore, ...) {
  aux.file <- pkg_files("pkgutils", what = "auxiliary")
  aux.file <- L(aux.file[tolower(basename(aux.file)) == tolower(script)])
  x <- pkg_files(x, what = "R", installed = FALSE, ignore = ignore)
  errs <- run_ruby(x = c(aux.file, x), ...)
  if (is.integer(errs) && !identical(errs, 0L))
    run_ruby(x = 1.9, ...) # to show Ruby version problems, if any
  errs
}


################################################################################


#' Prepare command-line options
#'
#' Simply routine to add leading dashes to strings for use as command-line
#' options. Works with options arguments, but only if these are separated from
#' the option by \sQuote{=}.
#'
#' @param x Character vector or \code{NULL}.
#' @return Character vector.
#' @keywords internal
#'
prepare_options <- function(x) UseMethod("prepare_options")

#' @rdname prepare_options
#' @method prepare_options NULL
#'
prepare_options.NULL <- function(x) {
  character()
}

#' @rdname prepare_options
#' @method prepare_options character
#'
prepare_options.character <- function(x) {
  x <- sub("^-+", "", x, perl = TRUE)
  len1 <- nchar(sub("=.*$", "", x, perl = TRUE)) == 1L
  x[len1] <- sprintf("-%s", x[len1])
  x[!len1] <- sprintf("--%s", x[!len1])
  x
}


################################################################################


#' Output some R object
#'
#' Generic function for outputting \R objects. Currently defined for some of
#' the classes relevant in this package.
#'
#' @inheritParams pack_desc
#' @param x Object of class \sQuote{class_desc}, \sQuote{classes_desc},
#'   \sQuote{pack_desc}, \sQuote{pack_descs} or \sQuote{Rd}.
#' @param file Character vector of file names to which the data in \code{x}
#'   shall be written.
#~ @export
#' @return \code{x}, returned invisibly.
#' @seealso base::cat base::write
#~ @family auxiliary-functions
#~ @keywords IO
#' @keywords internal
#~ @examples
#~ x <- class_rdfiles("methods", "ObjectsWithPackage", "content")
#~ y <- puts(x, "")
#~ stopifnot(identical(x, y))
#'
puts <- function(x, file, ...) UseMethod("puts")

#' @rdname puts
#' @method puts classes_desc
#' @export
#'
puts.classes_desc <- function(x, file, ...) {
  invisible(structure(mapply(FUN = puts, x = x, file = file,
    MoreArgs = list(...), SIMPLIFY = FALSE), class = oldClass(x)))
}

#' @rdname puts
#' @method puts class_desc
#' @export
#'
puts.class_desc <- function(x, file, ...) {
  cat(unlist(x), file = file, sep = "\n", ...)
  invisible(x)
}

#' @rdname puts
#' @method puts Rd
#' @export
#'
puts.Rd <- function(x, file, ...) {
  cat(as.character(x, deparse = TRUE), file = file, sep = "", ...)
  invisible(x)
}

#' @rdname puts
#' @method puts pack_desc
#' @export
#'
puts.pack_desc <- function(x, file, ...) {
  write.dcf(x = unclass(x), file = file, ...)
  invisible(x)
}

#' @rdname puts
#' @method puts pack_descs
#' @export
#'
puts.pack_descs <- function(x, file, ...) {
  invisible(structure(mapply(FUN = puts, x = x, file = file,
    MoreArgs = list(...), SIMPLIFY = FALSE), class = oldClass(x)))
}


################################################################################


#' Print message indicating problem
#'
#' Simple utility to print a message indicating a certain problem. Optionally
#' also note an input file in which the problem occurs.
#'
#' @param x Character vector.
#' @param infile Name of input file. Use \code{NULL} to ignore it.
#' @param line Number of input line. Use \code{NULL} to ignore it.
#' @export
#' @return Character vector.
#' @keywords internal
#'
problem <- function(x, ...) UseMethod("problem")

#' @rdname problem
#' @method problem character
#'
problem.character <- function(x, infile = NULL, line = NULL, ...) {
  infile <- sprintf(" '%s'", infile)
  line <- sprintf(" (line %i)", line)
  msg <- "PROBLEM in file"
  msg <- paste(msg, infile, line, ": ", x, sep = "", collapse = "\n")
  message(msg)
  if (nzchar(logfile <- get("logfile", PKGUTILS_OPTIONS)))
    cat(msg, sep = "\n", file = logfile, append = TRUE)
  invisible(NULL)
}


################################################################################


#' Load R files with source
#'
#' Load \R code files with \code{source} from the \pkg{base} package (which
#' does only handle one file at a time). This is mainly useful for loading
#' entire packages with \code{source}; the the \sQuote{what} argument of
#' \code{\link{pack_desc}} for an example.
#'
#' @param x Character vector of file names, or object of class
#'   \sQuote{pack_desc} or \sQuote{pack_descs}.
#' @param demo Logical scalar. See \code{\link{pack_desc}}.
#' @param ... Optional additional arguments passed between the methods and
#'   finally to \code{sys.source}.
#' @return List of lists with the results of calling \code{source}. For the
#'   action of the \sQuote{pack_desc} or \sQuote{pack_descs} methods, see
#'   \code{\link{pack_desc}}.
#~ @family auxiliary-functions
#~ @export
#' @keywords internal
#~ @examples
#~
#~ # character method
#~ stopifnot(!sapply(vars <- c("foo", "bar"), exists))
#~ tmpfile1 <- tempfile()
#~ tmpfile2 <- tempfile()
#~ cat("foo <- 1", file = tmpfile1)
#~ cat("bar <- 2", file = tmpfile2)
#~ source_files(c(tmpfile1, tmpfile2))
#~ stopifnot(sapply(vars, exists))
#~ rm(list = vars) # tidy up
#~
#~ # for the 'pack_desc' and 'pack_descs' methods, see pack_desc()
#'
source_files <- function(x, ...) UseMethod("source_files")

#' @method source_files character
#' @rdname source_files
#' @export
#'
source_files.character <- function(x, ...) {
  doit <- function(file) sys.source(file = file, ...)
  invisible(sapply(x, doit, simplify = FALSE))
}

#' @method source_files pack_descs
#' @rdname source_files
#' @export
#'
source_files.pack_descs <- function(x, ...) {
  invisible(sapply(X = x, FUN = source_files, ..., simplify = FALSE))
}

#' @method source_files pack_desc
#' @rdname source_files
#' @export
#'
source_files.pack_desc <- function(x, demo = FALSE, ...) {
  y <- subset(x)
  y <- list(depends = y$Depends, imports = y$Imports,
    r.files = file.path(dirname(attr(x, "file")), "R", y$Collate))
  if (L(demo))
    return(y)
  for (pkg in unlist(y[c("depends", "imports")]))
    suppressPackageStartupMessages(require(pkg, character.only = TRUE,
      quietly = TRUE, warn.conflicts = FALSE))
  invisible(source_files(x = y$r.files, ...))
}


################################################################################



