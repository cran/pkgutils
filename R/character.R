

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
    return(structure(factor(ordered = TRUE), .Names = names(x)))
  if (any(is.na(x)))
    stop("'x' must not contain NA values")
  result <- integer(length(x))
  true.runs <- x & c(x[-1L] == x[-length(x)], FALSE)
  result[!true.runs] <- prepare_sections(x[!true.runs])
  if (L(include))
    result[true.runs] <- NA_integer_
  else
    result[x] <- NA_integer_
  structure(as.ordered(result), .Names = names(x))
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



