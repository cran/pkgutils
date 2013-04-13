


################################################################################


#' The pkgutils package
#'
#' Tools useful when creating \R packages, particularly for checking the
#' documentation produced by \pkg{roxygen2} and for adding support for the
#' documentation of S4 methods, but also for checking some aspects of the
#' coding style. Further, some functions of use when coding a package and
#' others of use in interactive \R sessions. Finally, \R syntax highlighting
#' for some text editors.
#'
#' @name pkgutils.package
#' @aliases pkgutils
#' @docType package
#'
#' @details The package contains the following non-standard subdirectories:
#' \describe{
#'   \item{auxiliary}{Contains scripts in other programming languages that are
#'     called by some functions of this package. Some functionality of the
#'     \sQuote{docu.R} script needs them.}
#'   \item{highlighting}{Patterns for highlighting \R syntax for use with some
#'     text editors. See the files themselves for details.}
#'   \item{scripts}{Rscript scripts for non-interactive use of \R. The one used
#'     to create, check and modify package documentation is \sQuote{docu.R}.
#'     The other scripts have no connection to creating packages but might also
#'     be useful. See the help messages of these scripts (obtained via the
#'     \sQuote{--help} switch) for details. The scripts are expected to run at
#'     least on UNIX-like operating systems. Windows users might need to
#'     install Rtools, see \url{http://cran.r-project.org/bin/windows/Rtools/}.
#'     }
#' }
#~ @export
#' @keywords package
#'
NULL


################################################################################


# See check_keywords() for the use of this environment.
#
PKGUTILS_OPTIONS <- new.env(parent = emptyenv())
PKGUTILS_OPTIONS$logfile <- ""


# Directory stack used by swd(). See there.
#
DIRS <- new.env(parent = emptyenv())
DIRS$WD_INDEX <- 0L


################################################################################


SPECIAL_PAPER_SIZES <- structure(
  .Data = c(216, 432, 559, 864, 279, 457, 108, 381, 445, 572, 584, 184, 216,
    216, 210, 216, 216, 203, 140, 140, 127, 419, 279, 215.9, 215.9, 457, 140,
    184, 70, 279, 140, 216, 394, 889, 229, 508, 140, 330, 279, 279, 279, 559,
    864, 1118, 432, 610, 171, 508, 572, 889, 711, 267, 304, 330, 330, 304, 330,
    267, 216, 216, 203.2, 533, 432, 355.6, 279.4, 584, 216, 267, 127, 432, 216,
    279, 489, 1143, 279, 635, 216, 483, 432, 377),
  .Dim = c(40L, 2L),
  .Dimnames = list(c("ansi a", "ansi c", "ansi d", "ansi e", "bible",
    "broadsheet", "compact", "crown", "demy", "double demy", "elephant",
    "executive", "fanfold", "folio", "foolscap", "german std fanfold",
    "government legal", "government letter", "half letter", "jepps",
    "junior legal", "large post", "ledger", "legal", "letter", "medium",
    "memo", "monarch", "organizer j", "organizer k", "organizer l",
    "organizer m", "post", "quad demy", "quarto", "royal", "statement",
    "super b", "tabloid", "us std fanfold"), c("width", "height"))
)


################################################################################


# TODO: this still does not cover all cases
#
QUOTED <- "(%s(\\\\\"|[^\"])*%s|%s[^`]+%s|%s(\'|[^'])*%s)"

QUOTED_END <- sprintf(QUOTED, '"', "$", "`", "$", "'", "$")

QUOTED_BEGIN <- sprintf(QUOTED, "^", '"', "^", "`", "^", "'")

QUOTED <- sprintf(QUOTED, '"', '"', "`", "`", "'", "'")

OPS_LEFT <- paste("[^\\s]([~/^]|%[^%]*%)", "[^\\s*][*]", "[^\\s<]<",
  "[^\\s[(:][+]", "[^\\s<[(:]-", "[^\\s>-]>", "[^\\s<>=!]=", "[^\\s&]&",
  "[^\\s|]\\|", sep = "|")

# We cannot check by what '-' and '+' are followed because these are also unary
# operators.
#
OPS_RIGHT <- paste("([~/^]|%[^%]*%)[^\\s]", "[*][^\\s*]", "<[^\\s<=-]",
  ">[^\\s>=]", "=[^\\s=,]", "&[^\\s&]", "\\|[^\\s|]", sep = "|")


################################################################################


GREGEXPR_NO_MATCH <- structure(-1L, match.length = -1L)


################################################################################


