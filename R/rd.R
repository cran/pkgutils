repair_docu <- function(x, ...) UseMethod("repair_docu")

repair_docu.character <- function(x, ignore = NULL, drop.internal = FALSE,
    ...) {
  do_repair <- function(x) {
    data <- repair_docu(parse_Rd(file = x), ...)
    kw <- check_keywords(data, file = x, list.only = FALSE)
    check_examples(data, file = x)
    if (drop.internal)
      if (identical(kw, "internal")) {
        unlink(x)
        TRUE
      } else {
        puts(data, file = x)
        FALSE
      }
    else
      puts(data, file = x)
  }
  LL(drop.internal)
  x <- pkg_files(x, what = "man", installed = FALSE, ignore = ignore)
  invisible(sapply(x, do_repair, simplify = drop.internal))
}

repair_docu.Rd <- function(x, remove.dups = FALSE, text.dups = FALSE,
    infile = attr(attr(x, "srcref"), "srcfile")$filename, ...) {
  dup_str <- function(...) {
    x <- paste0(..., collapse = " ")
    m <- gregexpr("\\b(\\w+)(?:\\s+\\1\\b)+", x, FALSE, TRUE)
    unlist(regmatches(x, m), FALSE, FALSE)
  }
  cum_parts <- function(x) {
    x <- strsplit(x, ".", TRUE)
    x <- x[vapply(x, length, 0L) > 0L]
    unlist(lapply(x, function(y) {
      vapply(seq_along(y),
        function(i) paste0(y[seq.int(1L, i)], collapse = "."), "")
    }))
  }
  function.names <- new.env(parent = emptyenv())
  removed <- FALSE
  do_repair <- function(x, parent.tags) {
    case(attr(x, "Rd_tag"),
      TEXT = {
        switch(parent.tags[1L],
          `\\keyword` = x[x == "dataset"] <- "datasets",
          `\\link` = if (remove.dups && "\\seealso" %in% parent.tags[-1L]) {
            for (part in cum_parts(x))
              if (exists(part, function.names)) {
                x <- NULL
                removed <<- TRUE
                break
              }
            if (!is.null(x)) {
              removed <<- FALSE
              function.names[[x]] <- NULL
            }
          },
          `\\seealso` = if (removed) {
            x <- NULL
            removed <<- FALSE
          },
          if (text.dups && length(x))
            if (length(dup <- dup_str(x))) {
              dup <- listing(dup, style = I("'%s'"), collapse = ", ")
              problem(paste("duplicated words:", dup), infile)
            }
        )
        x
      },
      COMMENT =,
      VERB = x,
      RCODE = {
        switch(parent.tags[1L],
          `\\usage` = {
            if (grepl("\\s*<-\\s*value\\s*$", x, FALSE, TRUE))
            # Repair duplicate 'value' entries of replacement functions
              x <- sub(",\\s*value", "", x, FALSE, TRUE)
            # break long lines
            x <- gsub("(.{80})\\s", "\\1\n    ", x, FALSE, TRUE)
          }
        )
        x
      }
    )
  }
  repair_recursively <- function(x, parent.tags) {
    if (!is.list(x))
      return(do_repair(x, parent.tags))
    if (!length(x))
      return(x) # keep lists as-is if they were already empty
    if (is.null(this.tag <- attr(x, "Rd_tag")))
      this.tag <- ".empty"
    y <- lapply(x, repair_recursively, parent.tags = c(this.tag, parent.tags))
    y[vapply(y, is.null, NA)] <- NULL
    if (!length(y))
      return(NULL)
    attributes(y) <- attributes(x)
    y
  }
  LL(remove.dups, text.dups)
  repair_recursively(x, ".toplevel")
}

check_keywords <- function(x, ...) UseMethod("check_keywords")

check_keywords.NULL <- function(x, full = FALSE, ...) {
  x <- readLines(file.path(R.home("doc"), "KEYWORDS.db"))
  x <- grep("|", x, FALSE, FALSE, TRUE, TRUE)
  x <- sub("^.*\\|", "", x, FALSE, TRUE)
  x <- do.call(rbind, strsplit(x, "\\s*:\\s*", FALSE, TRUE))
  if (L(full))
    return(structure(x[, 2L], names = x[, 1L]))
  sort.int(unique.default(x[, 1L]))
}

check_keywords.character <- function(x, ...) {
  check_keywords(x = NULL, full = TRUE, ...)[x]
}

check_keywords.logical <- function(x, ...) {
  check_keywords(x = NULL, full = x, ...)
}

check_keywords.Rd <- function(x, file = NULL, list.only = FALSE, ...) {
  if (!length(kw <- which(subset(x, "keyword")))) {
    if (L(list.only)) {
      problem("no keywords", file)
      return(character())
    } else
      return(invisible(character()))
  }
  kw <- unlist(x[kw])
  if (L(list.only))
    return(kw)
  if (exists("rd.keywords", PKGUTILS_OPTIONS))
    known.kw <- PKGUTILS_OPTIONS$rd.keywords
  else
    PKGUTILS_OPTIONS$rd.keywords <- known.kw <- check_keywords()
  if (length(bad <- setdiff(kw, known.kw)))
    problem(paste("unknown keywords:", paste0(bad, collapse = ", ")), file)
  else if ("internal" %in% kw && length(kw) > 1L)
    problem("'internal' combined with other keyword(s)", file)
  else if (length(doctype <- unlist(subset(x, "docType", values = TRUE))))
    switch(doctype,
      class = if (!all(c("classes", "methods") %in% kw))
        problem("'class' docType but no 'classes' or no 'methods' keyword",
          file),
      data = if (!"datasets" %in% kw)
        problem("'data' docType but no 'datasets' keyword", file),
      package = if (!"package" %in% kw)
        problem("'package' docType but no 'package' keyword", file)
    )
  invisible(kw)
}

check_examples <- function(x, ...) UseMethod("check_examples")

check_examples.Rd <- function(x, file = NULL, ...) {
  kw <- check_keywords(x = x, file = file, list.only = TRUE, ...)
  if (length(ex <- which(subset(x, "examples"))) && "internal" %in% kw) {
    problem("'internal' keyword but examples present", file)
    FALSE
  } else if (!length(ex) && !"internal" %in% kw) {
    doctype <- unlist(subset(x, "docType", values = TRUE))
    if (all(doctype %in% c("class", "package")))
      TRUE
    else {
      problem("no 'internal' keyword but examples missing", file)
      FALSE
    }
  } else
    TRUE
}

repair_S4_docu <- function(x, ...) UseMethod("repair_S4_docu")

repair_S4_docu.character <- function(x, ..., ignore = NULL) {
  run_pkgutils_ruby(x = x, script = "repair_S4_docu.rb", ignore = ignore, ...)
}

