

################################################################################


#' Create sections
#'
#' The \sQuote{logical} method treats a logical vector by regarding \code{TRUE}
#' as indicating separation. It creates a factor that could be used with
#' \code{split} to split the logical vector, or any equal-length object from
#' which it was created, into according groups. The \sQuote{character} method
#' splits a character vector according to a pattern or to a given output
#' substring length.
#'
#' @inheritParams pack_desc
#' @param x Logical vector. It is an error if \code{NA} values are contained.
#' @param include Logical scalar indicating whether the separator positions
#'   should also be included in the factor levels instead of being coded as
#'   \code{NA}.
#' @param pattern Scalar. If of mode \sQuote{character}, passed to
#'   \code{grepl} from the \pkg{base} package. If numeric, used to indicate the
#'   lengths of the substrings to extract.
#' @param invert Negate the results of \code{grepl}?
#' @param perl Logical scalar passed to \code{grepl}.
#' @return The \sQuote{logical} method returns an
#'   ordered factor, its length being the one of \code{x}. The levels
#'   correspond to a groups whose indices correspond to the index of a
#'   \code{TRUE} value in \code{x} plus the indices of the \code{FALSE}
#'   values immediately following it. \sQuote{logical} method returns a list
#'   of character vectors.
#' @details When applying \code{split}, positions corresponding to \code{NA}
#'   factor levels will usually be removed. Thus note the action of the
#'   \code{include} argument, and note that the positions of \code{TRUE} values
#'   that are followed by other \code{TRUE} values are always set to \code{NA},
#'   irrespective of \code{include}. The \sQuote{character} method using a
#'   pattern works by passing this pattern to \code{grepl}, the result to
#'   the \sQuote{logical} method and this in turn to \code{split}.
#'
#' @seealso base::split base::grepl
#' @export
#' @family character-functions
#' @keywords utilities character
#' @examples
#'
#' ## 'logical' method
#'
#' # clean input
#' x <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, 1, 1, 2, 2, 3, 3)), y))
#'
#' # now exclude the separators
#' y <- sections(x, include = FALSE)
#' stopifnot(identical(as.ordered(c(NA, 1, 1, NA, 2, NA, 3)), y))
#'
#' # leading FALSE
#' x <- c(FALSE, x)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, 2, 2, 2, 3, 3, 4, 4)), y))
#'
#' # adjacent TRUEs and trailing TRUE
#' x <- c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, NA, 2, 2, 3, 3, NA, 4, 4, 5)), y))
#'
#' # several adjacent TRUEs
#' x <- c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
#' (y <- sections(x))
#' stopifnot(identical(as.ordered(c(1, NA, NA, NA, 2, 2, 3, 3)), y))
#'
#' ## 'character' method
#'
#' # using a specified length
#' x <- c("abcdef", "ghijk")
#' (y <- sections(x, 2))
#' stopifnot(is.list(y), length(y) == 2)
#' stopifnot(y[[1]] == c("ab", "cd", "ef"), y[[2]] == c("gh", "ij", "k"))
#'
#' # using a regexp pattern
#' x <- c(">abc", ">def", "acgtagg", ">hij", "gatattag", "aggtagga") # FASTA
#' (y <- sections(x, "^>", include = TRUE))
#' stopifnot(identical(y, list(`1` = x[2:3], `2` = x[4:6])))
#' (y <- sections(x, "^>", include = FALSE))
#' stopifnot(identical(y, list(`1` = x[3], `2` = x[5:6])))
#'
sections <- function(x, ...) UseMethod("sections")

#' @rdname sections
#' @method sections logical
#' @export
#'
sections.logical <- function(x, include = TRUE, ...) {
  prepare_sections <- function(x) {
    if (prepend <- !x[1L])
      x <- c(TRUE, x)
    if (append <- x[length(x)])
      x <- c(x, FALSE)
    x <- matrix(cumsum(rle(x)$lengths), ncol = 2L, byrow = TRUE)
    x <- x[, 2L] - x[, 1L] + 1L
    x <- mapply(rep.int, seq_along(x), x, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    x <- unlist(x)
    if (prepend)
      x <- x[-1L]
    if (append)
      x <- x[-length(x)]
    x
  }
  if (!(n <- length(x)))
    return(structure(factor(ordered = TRUE), names = names(x)))
  if (any(is.na(x)))
    stop("'x' must not contain NA values")
  result <- integer(length(x))
  true.runs <- x & c(x[-1L] == x[-length(x)], FALSE)
  result[!true.runs] <- prepare_sections(x[!true.runs])
  if (L(include))
    result[true.runs] <- NA_integer_
  else
    result[x] <- NA_integer_
  structure(as.ordered(result), names = names(x))
}

#' @rdname sections
#' @method sections character
#' @export
#'
sections.character <- function(x, pattern, invert = FALSE, include = TRUE,
    perl = TRUE, ...) {
  if (is.character(pattern)) {
    found <- grepl(pattern = pattern, x = x, perl = perl, ...)
    if (L(invert))
      found <- !found
    split.default(x, sections(found, include))
  } else if (is.numeric(pattern)) {
    if (identical(pattern <- as.integer(pattern), 1L))
      return(strsplit(x, "", fixed = TRUE))
    pattern <- sprintf("(.{%i,%i})", pattern, pattern)
    strsplit(gsub(pattern, "\\1\a", x, perl = TRUE), "\a", fixed = TRUE)
  } else
    stop("'pattern' must be a character or numeric scalar")
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
#' @family character-functions
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
    return(structure(logical(), names = character(), errors = character()))
  result <- do.call(rbind, lapply(x, doit))
  structure(unlist(result[, 1L]), names = x, errors = unlist(result[, 2L]))
}


################################################################################



