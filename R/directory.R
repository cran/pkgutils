

################################################################################


#' Change or list working directories
#'
#' Set the working directory to, e.g., a parent directory of the current one, or
#' to a directory visited earlier. Alternatively, list the working directories
#' stored by using \code{\link{swd}}, or just the current working directory if
#' \code{\link{swd}} has not been called yet. These is mainly convenience
#' functions for interactive sessions.
#'
#' @param x For \code{swd}, a numeric scalar indicating how often to move
#'   upwards (i.e., to which parent directory, or character vector containing
#'   directory names, or \code{NULL}. If \code{x} is a negative number, this is
#'   used to go to one of the working directories used earlier, using an
#'   internally stored directory list. That is, if \code{n} is a numeric scalar,
#'   the action of \code{swd(n)} is not necessarily the inverse of what
#'   \code{swd(-n)} is doing.
#'
#'   If \code{x} is a character vector, its elements passed in turn to
#'   \code{setwd}.
#'
#'   For \code{listwd}, \code{x} is an optional numeric scalar indicating how
#'   many directories (maximally) to show. The default is 10.
#' @export
#' @return \code{swd} yields \code{NULL}, returned invisibly. As a side effect,
#'   the name of the resulting working directory is printed. This is the only
#'   action if \code{x} is \code{NULL}. The directory stack registers a new
#'   directory only via calls to \code{swd} itself, not via \code{setwd}.
#'
#'   For \code{listwd}, a character vector with directory names (current one
#'   last), returned invisibly. As a side effect, the list of at most \code{x}
#'   last directories is printed together with the numeric indexes that would be
#'   needed to set them using \code{\link{swd}}, respectively.
#'
#' @keywords environment
#' @seealso base::setwd base::getwd
#' @family directory-functions
#' @examples
#'
#' ## listwd()
#' (d1 <- getwd())
#' x <- listwd()
#' stopifnot(x == d1)
#' swd(1)
#' x <- listwd()
#' stopifnot(x == c(d1, dirname(d1)))
#' swd(-1)
#' x <- listwd()
#' stopifnot(x == c(d1, dirname(d1), d1))
#'
#' ## swd()
#' (d1 <- getwd())
#' swd(1) # got to immediate parent directory
#' stopifnot(d1 != getwd(), dirname(d1) == getwd())
#' swd(d1) # go back, using a name
#' stopifnot(d1 == getwd())
#' swd(1) # go upwards again
#' stopifnot(d1 != getwd(), dirname(d1) == getwd())
#' swd(-1) # go back, using the position within the visited directories
#' stopifnot(d1 == getwd())
#' swd(-2) # go back, using the position again
#' stopifnot(d1 == getwd())
#'
swd <- function(x) UseMethod("swd")

#' @rdname swd
#' @method swd NULL
#' @export
#'
swd.NULL <- function(x) {
  if (!exists("WD_0", DIRS))
    DIRS$WD_0 <- getwd()
  message(getwd())
}

#' @rdname swd
#' @method swd character
#' @export
#'
swd.character <- function(x) {
  if (identical(wd.index <- DIRS$WD_INDEX, 0L) && !exists("WD_0", DIRS))
    DIRS$WD_0 <- getwd()
  for (dir.name in x)
    setwd(dir.name)
  message(new.wd <- getwd())
  old.wd <- DIRS[[sprintf("WD_%i", wd.index)]]
  if (!identical(old.wd, new.wd)) {
    wd.index <- DIRS$WD_INDEX <- wd.index + 1L
    DIRS[[sprintf("WD_%i", wd.index)]] <- new.wd
  }
  invisible(NULL)
}

#' @rdname swd
#' @method swd numeric
#' @export
#'
swd.numeric <- function(x) {
  if (L(x <- as.integer(x)) > 0L) {
    y <- getwd()
    for (i in seq.int(1L, x))
      y <- dirname(y)
  } else {
    wd.index <- DIRS$WD_INDEX + x
    y <- DIRS[[sprintf("WD_%i", wd.index)]]
  }
  swd.character(y)
}

#' @rdname swd
#' @export
#'
listwd <- function(x) UseMethod("listwd")

#' @rdname swd
#' @method listwd NULL
#' @export
#'
listwd.NULL <- function(x) {
  listwd.numeric(10)
}

#' @rdname swd
#' @method listwd numeric
#' @export
#'
listwd.numeric <- function(x) {
  if (!exists("WD_0", DIRS))
    DIRS$WD_0 <- getwd()
  y <- -seq.int(pmin(L(x <- as.integer(x)) - 1L, DIRS$WD_INDEX), 0L)
  x <- unlist(mget(sprintf("WD_%i", DIRS$WD_INDEX + y), DIRS))
  message(paste(sprintf("% 3i", y), x, collapse = "\n"))
  invisible(x)
}


################################################################################

