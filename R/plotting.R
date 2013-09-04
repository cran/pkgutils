

################################################################################


#' Paper size for PDF files
#'
#' \code{mypdf} is a wrapper for \code{pdf} from the \pkg{grDevices} package.
#' The difference is that \code{mypdf} determines the width and the height of
#' the plotting region from a given standard paper format using
#' \code{paper_size}. \code{max_rgb_constrast} is a helper functions for
#' arranging colours.
#'
#' @param file Passed as \sQuote{file} argument to \code{pdf} from the
#'   \pkg{grDevices} package. See there for details.
#' @param paper Character scalar like the eponymous argument of \code{pdf},
#'   but here it is passed to \code{\link{paper_size}} to determine the
#'   \sQuote{width} and the \sQuote{height} of the plotting region.
#' @param prop Numeric vector. Its values should be between 0 and 1. Its first
#'   element is multiplied with the width of \code{paper} to yield the width of
#'   the plotting region. Its last element is multiplied with the height of
#'   \code{paper} to yield the height of the plotting region.
#'
#' @param x Numeric or character vector for determining the paper size. If a
#'   numeric vector, specifying paper sizes in the \sQuote{DIN} series. If a
#'   character vector, the full names of well-known paper formats such as
#'   \sQuote{a4}, \sQuote{letter}, etc.
#'
#'   For \code{max_rgb_contrast}, the names or hexadecimal codes of the colours
#'   to be sorted. Might also be an integer vector, see \code{col2rgb} from the
#'   \pkg{grDevices} package for details. Duplicate RGB coordinates and unknown
#'   names will cause an error.
#'
#' @param landscape Logical scalar. If \code{FALSE}, \sQuote{portrait} paper
#'   orientation is assumed. For the character method, this has only an effect
#'   for paper size specifiers such as \sQuote{letter} that do not belong to
#'   the \sQuote{DIN} series. For the \sQuote{DIN} series, append \sQuote{R} to
#'   the specifier to obtain \sQuote{landscape} orientation.
#' @param inches Logical scalar. If \code{TRUE}, output unit is inches,
#'   otherwise millimeters.
#' @param series Character scalar indicating the \sQuote{DIN} series to assume.
#' @param ... Optional arguments passed to other methods, e.g., to \code{pdf}
#'   from the \pkg{grDevices} package.
#'
#' @export
#' @return \code{mypdf} returns \code{NULL}. As a side effect, \code{file} is
#'   opened. \code{paper_size} yields a numeric matrix with columns
#'   \sQuote{width} and \sQuote{height} and \code{x} as row names (if it was a
#'   character vector). \code{max_rgb_contrast} returns a character vector (the
#'   rearranged input names).
#' @details The computation of the paper size is done numerically for the
#'   \acronym{DIN} series, whereas a lookup table is used for the other formats.
#'
#'   \code{max_rgb_contrast} arranges colours so as to achieve that neighboring
#'   colours are most distinct with respect to their RGB coordinates. This is
#'   done as follows: (1) Euclidean distances between the RGB coordinates of the
#'   input colours are calculated; (2) the distances are logarithmized and
#'   inversed; (3) a principal-coordinate analysis is conducted on these
#'   inversed distances; (4) the input colours are sorted according to the first
#'   principal coordinate.
#'
#'   Note that this is probably only works for colour vectors of small to
#'   moderate size, and that the resulting vector could as well be used in
#'   reverse order (see the examples).
#'
#' @references \url{http://en.wikipedia.org/wiki/Paper_size}
#'
#' @family plotting-functions
#' @seealso grDevices::pdf grDevices::col2rg
#' @keywords IO dplot color
#' @examples
#' \dontrun{
#'   mypdf("example.pdf")
#'   ## create some plots...
#'   dev.off()
#' }
#'
#' ## paper_size()
#'
#' query <- c("A4", "Letter", "unknown")
#' (x <- paper_size(query))
#' stopifnot(is.matrix(x), is.numeric(x), rownames(x) == query)
#' stopifnot(colnames(x) == c("height", "width"), is.na(x["unknown", ]))
#' (y <- paper_size(4))
#' stopifnot(identical(y, x[1L, , drop = FALSE]))
#'
#' ## max_rgb_contrast()
#'
#' # with colours
#' (x <- max_rgb_contrast(c("darkred", "darkblue", "blue", "red")))
#' y <- c("darkblue", "red", "blue", "darkred")
#' stopifnot(identical(x, y) || identical(x, rev(y)))
#'
#' # shades of grey 1
#' (x <- max_rgb_contrast(c("white", "grey", "black")))
#' y <- c("grey", "black", "white")
#' stopifnot(identical(x, y) || identical(x, rev(y)))
#'
#' # shades of grey 2
#' (x <- max_rgb_contrast(c("white", "darkgrey", "lightgrey", "black")))
#' y <- c("lightgrey", "black", "white", "darkgrey")
#' stopifnot(identical(x, y) || identical(x, rev(y)))
#'
mypdf <- function(file, ...) UseMethod("mypdf")

#' @rdname mypdf
#' @method mypdf character
#' @export
#'
mypdf.character <- function(file, paper = "a4r", prop = 0.9, ...) {
  paper.size <- paper_size(paper)
  width <- prop[1L] * paper.size[, "width"]
  height <- prop[length(prop)] * paper.size[, "height"]
  pdf(file = file, paper = paper, width = width, height = height, ...)
}

#' @rdname mypdf
#' @export
#'
paper_size <- function(x, ...) UseMethod("paper_size")

#' @rdname mypdf
#' @method paper_size numeric
#' @export
#'
paper_size.numeric <- function(x, landscape = FALSE, series = c("A", "B", "C"),
    ...) {
  pattern <- sprintf("%s%%i", match.arg(series))
  must(pattern[landscape] <- sprintf("%sR", pattern[landscape]))
  paper_size(sprintf(pattern, abs(x)), ...)
}

#' @rdname mypdf
#' @method paper_size character
#' @export
#'
paper_size.character <- function(x, landscape = FALSE, inches = FALSE, ...) {
  parse_din_string <- function(x) {
    get_orientation <- function(x) {
      x <- toupper(sub("^.*\\d", "", x, perl = TRUE))
      x[!nzchar(x)] <- "L"
      x
    }
    get_series <- function(x) toupper(substr(x, 1L, 1L))
    get_size <- function(x) {
      as.integer(gsub("[A-Z]", "", x, perl = TRUE, ignore.case = TRUE))
    }
    y <- gsub("\\W", "", x, perl = TRUE)
    data.frame(series = get_series(y), size = get_size(y),
      orientation = get_orientation(y), row.names = x,
      stringsAsFactors = FALSE)
  }
  long_size_in_mm <- function(series, size) {
    get_size <- function(n, m) 0.2 + 1000 * 2 ^ -(0.5 * n - m)
    m <- numeric(length = length(series))
    m[series == "A"] <- 0.25
    m[series == "B"] <- 0.5
    m[series == "C"] <- 0.375
    m[m == 0] <- NA_real_
    get_size(n = size, m = m)
  }
  LL(inches, landscape)
  x <- parse_din_string(x)
  x$height <- long_size_in_mm(x$series, x$size)
  across <- x$orientation == "R"
  x[!across, "width"] <- x[!across, "height"] / sqrt(2)
  x[across, "width"] <- x[across, "height"]
  x[across, "height"] <- x[across, "width"] / sqrt(2)
  x <- as.matrix(x[, 4L:5L])
  wanted <- is.na(x[, 1L]) &
    tolower(rownames(x)) %in% rownames(SPECIAL_PAPER_SIZES)
  x[wanted, ] <- SPECIAL_PAPER_SIZES[tolower(rownames(x)[wanted]), 2L:1L]
  if (landscape)
    x[wanted, ] <- x[wanted, 2L:1L]
  if (inches)
    x <- x / 25.4
  x
}

#' @rdname mypdf
#' @export
#'
max_rgb_contrast <- function(x, ...) UseMethod("max_rgb_contrast")

#' @rdname mypdf
#' @method max_rgb_contrast default
#' @export
#'
max_rgb_contrast.default <- function(x, ...) {
  col.rgb <- t(col2rgb(x))
  rownames(col.rgb) <- x
  pco <- cmdscale(1 / log(dist(col.rgb)), k = 1L)
  names(sort.int(pco[, 1L]))
}


################################################################################

