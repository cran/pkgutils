mypdf <- function(file, ...) UseMethod("mypdf")

mypdf.character <- function(file, paper = "a4r", prop = 0.9, ...) {
  paper.size <- paper_size(paper)
  width <- prop[1L] * paper.size[, "width"]
  height <- prop[length(prop)] * paper.size[, "height"]
  pdf(file = file, paper = paper, width = width, height = height, ...)
}

paper_size <- function(x, ...) UseMethod("paper_size")

paper_size.numeric <- function(x, landscape = FALSE, series = c("A", "B", "C"),
    ...) {
  pattern <- sprintf("%s%%i", match.arg(series))
  must(pattern[landscape] <- sprintf("%sR", pattern[landscape]))
  paper_size(sprintf(pattern, abs(x)), ...)
}

paper_size.character <- function(x, landscape = FALSE, inches = FALSE, ...) {
  parse_din_string <- function(x) {
    get_orientation <- function(x) {
      x <- toupper(sub("^.*\\d", "", x, FALSE, TRUE))
      x[!nzchar(x)] <- "L"
      x
    }
    get_series <- function(x) toupper(substr(x, 1L, 1L))
    get_size <- function(x) as.integer(gsub("[A-Z]", "", x, TRUE, TRUE))
    y <- gsub("\\W", "", x, FALSE, TRUE)
    data.frame(series = get_series(y), size = get_size(y), row.names = x,
      orientation = get_orientation(y), stringsAsFactors = FALSE)
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

max_rgb_contrast <- function(x, ...) UseMethod("max_rgb_contrast")

max_rgb_contrast.default <- function(x, ...) {
  col.rgb <- t(col2rgb(x))
  rownames(col.rgb) <- x
  pco <- cmdscale(1 / log(dist(col.rgb)), k = 1L)
  names(sort.int(pco[, 1L]))
}

