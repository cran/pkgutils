run_ruby <- function(x, ...) UseMethod("run_ruby")

run_ruby.NULL <- function(x, ruby = "ruby", ...) {
  unname(Sys.which(L(ruby)))
}

run_ruby.numeric <- function(x, args = "-w", ruby = "ruby", ...) {
  y <- "-e'raise \"need Ruby %.1f.0 or higher\" if RUBY_VERSION.to_f < %.1f'"
  args <- c(sprintf(y, x, x), args)
  run_ruby(x = character(), args = args, ruby = ruby, ...)
}

run_ruby.character <- function(x, args = "-w", ruby = "ruby", ...) {
  if (!nzchar(ruby <- run_ruby(x = NULL, ruby = ruby)))
    stop(sprintf("cannot find executable '%s'", ruby))
  args <- c(setdiff(as.character(args), "--"), "--")
  command <- paste0(c(ruby, args, x), collapse = " ")
  do.call(system, list(command = command, ...))
}

rd_quote <- function(x, ...) UseMethod("rd_quote")

rd_quote.character <- function(x, text, ...) {
  text <- toRd(na.fail(text))
  x <- x[nzchar(na.fail(x))]
  if (length(bad <- x[x != toRd(x)]))
    stop(sprintf("command name '%s' contains special characters", bad[1L]))
  for (item in rev(x))
    text <- sprintf("\\%s{%s}", item, text)
  text
}

NULL

subset.Rd <- function(x, subset, values = FALSE, ...) {
  prepend <- !grepl("^\\\\", subset, FALSE, TRUE)
  subset[prepend] <- sprintf("\\%s", subset[prepend])
  y <- vapply(x, attr, "", which = "Rd_tag") %in% subset
  if (L(values)) {
    x[!y] <- NULL
    x
  } else
    y
}

subset.pack_desc <- function(x, ...) {
  result <- c("Depends", "Imports", "Suggests", "Enhances", "Collate")
  result <- structure(vector("list", length(result)), names = result)
  for (name in c("Depends", "Imports", "Suggests", "Enhances"))
    if (!is.null(y <- x[[name]])) {
      y <- unlist(strsplit(y, "\\s*,\\s*", FALSE, TRUE), FALSE, FALSE)
      y <- sub("\\s+$", "", sub("^\\s+", "", y, FALSE, TRUE), FALSE, TRUE)
      y <- sub("\\s*\\([^)]*\\)$", "", y, FALSE, TRUE)
      result[[name]] <- y[y != "R"]
    }
  for (name in "Collate")
    if (!is.null(y <- x[[name]])) {
      y <- unlist(strsplit(y, "\\s+", FALSE, TRUE))
      result[[name]] <- gsub('"', "", gsub("'", "", y, FALSE, FALSE, TRUE),
        FALSE, FALSE, TRUE)
    }
  result
}

subset.pack_descs <- function(x, ...) {
  lapply(X = x, FUN = subset, ...)
}

run_pkgutils_ruby <- function(x, ...) UseMethod("run_pkgutils_ruby")

run_pkgutils_ruby.character <- function(x, script, ignore, sargs = NULL, ...) {
  aux.file <- pkg_files("pkgutils", what = "auxiliary")
  aux.file <- L(aux.file[tolower(basename(aux.file)) == tolower(script)])
  x <- pkg_files(x, what = "R", installed = FALSE, ignore = ignore)
  errs <- run_ruby(x = c(aux.file, x, prepare_options(sargs)), ...)
  if (is.integer(errs) && !identical(errs, 0L))
    run_ruby(x = 1.9, ...) # to show Ruby version problems, if any
  errs
}

prepare_options <- function(x) UseMethod("prepare_options")

prepare_options.NULL <- function(x) {
  character()
}

prepare_options.character <- function(x) {
  x <- sub("^-+", "", x, FALSE, TRUE)
  len1 <- nchar(sub("=.*$", "", x, FALSE, TRUE)) == 1L
  x[len1] <- sprintf("-%s", x[len1])
  x[!len1] <- sprintf("--%s", x[!len1])
  x
}

puts <- function(x, file, ...) UseMethod("puts")

puts.classes_desc <- function(x, file, ...) {
  invisible(structure(mapply(FUN = puts, x = x, file = file,
    MoreArgs = list(...), SIMPLIFY = FALSE), class = oldClass(x)))
}

puts.class_desc <- function(x, file, ...) {
  cat(unlist(x), file = file, sep = "\n", ...)
  invisible(x)
}

puts.Rd <- function(x, file, ...) {
  cat(as.character(x, deparse = TRUE), file = file, sep = "", ...)
  invisible(x)
}

puts.pack_desc <- function(x, file, ...) {
  write.dcf(x = unclass(x), file = file, ...)
  invisible(x)
}

puts.pack_descs <- function(x, file, ...) {
  invisible(structure(mapply(FUN = puts, x = x, file = file,
    MoreArgs = list(...), SIMPLIFY = FALSE), class = oldClass(x)))
}

problem <- function(x, ...) UseMethod("problem")

problem.character <- function(x, infile = NULL, line = NULL, ...) {
  infile <- sprintf(" '%s'", infile)
  line <- sprintf(" (line %i)", line)
  msg <- "PROBLEM in file"
  msg <- paste0(msg, infile, line, ": ", x, collapse = "\n")
  message(msg)
  if (nzchar(logfile <- get("logfile", PKGUTILS_OPTIONS)))
    cat(msg, sep = "\n", file = logfile, append = TRUE)
  invisible(NULL)
}

source_files <- function(x, ...) UseMethod("source_files")

source_files.character <- function(x, ...) {
  doit <- function(file) sys.source(file = file, ...)
  invisible(sapply(x, doit, simplify = FALSE))
}

source_files.pack_descs <- function(x, demo = FALSE, ...) {
  result <- sapply(X = x, FUN = source_files.pack_desc, demo = demo, ...,
    simplify = FALSE)
  if (L(demo))
    result
  else
    invisible(result)
}

source_files.pack_desc <- function(x, demo = FALSE, ...) {
  y <- subset(x)[c("Depends", "Imports", "Collate")]
  y$Collate <- file.path(dirname(attr(x, "file")), "R", y$Collate)
  if (L(demo))
    return(y)
  for (pkg in unlist(y[c("Depends", "Imports")]))
    suppressPackageStartupMessages(require(pkg, character.only = TRUE,
      quietly = TRUE, warn.conflicts = FALSE))
  invisible(source_files.character(x = y$Collate, ...))
}

