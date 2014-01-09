sections <- function(x, ...) UseMethod("sections")

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

sections.character <- function(x, pattern, invert = FALSE, include = TRUE,
    perl = TRUE, ...) {
  if (is.character(pattern)) {
    found <- grepl(pattern = pattern, x = x, perl = perl, ...)
    if (L(invert))
      found <- !found
    split.default(x, sections(found, include))
  } else if (is.numeric(pattern)) {
    if (identical(pattern <- as.integer(pattern), 1L))
      return(strsplit(x, "", TRUE))
    pattern <- sprintf("(.{%i,%i})", pattern, pattern)
    strsplit(gsub(pattern, "\\1\a", x, FALSE, TRUE), "\a", TRUE)
  } else
    stop("'pattern' must be a character or numeric scalar")
}

map_files <- function(x, ...) UseMethod("map_files")

map_files.character <- function(x, mapfun, ..., .attr = ".filename",
    .encoding = "", .sep = NULL, .warn = FALSE) {
  doit <- function(filename) tryCatch({
    add_attr <- function(x) {
      attr(x, .attr) <- filename
      x
    }
    connection <- file(description = filename, encoding = .encoding)
    x <- readLines(con = connection, warn = .warn)
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
      if (grepl("windows", Sys.info()[["sysname"]], TRUE, TRUE))
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

