case <- function(EXPR, ...) UseMethod("case")

case.numeric <- function(EXPR, ...) {
  stopifnot(EXPR >= 0L, nargs() > 1L)
  switch(EXPR = pmin(EXPR, nargs() - 2L) + 1L, ...)
}

case.character <- function(EXPR, ...) {
  switch(EXPR = EXPR, ..., stop("unmatched 'EXPR' value"))
}

must <- function(expr, msg = NULL, ..., domain = NULL) {
  # For some reason, using stop() directly results in errors that cannot be
  # catched with tryCatch() any more.
  tryCatch(expr = expr, warning = function(w) stop(if (length(msg))
    msg
  else
    conditionMessage(w), call. = FALSE, domain = domain), ...)
}

L <- function(x, .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  if (identical(length(x), .wanted))
    return(x)
  stop(sprintf(.msg, deparse(match.call()$x), .wanted), call. = FALSE,
    domain = .domain)
}

LL <- function(..., .wanted = 1L, .msg = "need object '%s' of length %i",
    .domain = NULL) {
  arg.names <- as.character(match.call())[-1L][seq_along(items <- list(...))]
  invisible(mapply(function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf(.msg, name, .wanted), call. = FALSE, domain = .domain)
    name
  }, items, arg.names, SIMPLIFY = TRUE, USE.NAMES = FALSE))
}

listing <- function(x, ...) UseMethod("listing")

listing.double <- function(x, ...) {
  x <- signif(x, getOption("digits"))
  mode(x) <- "character"
  listing.character(x, ...)
}

listing.factor <- function(x, ...) {
  y <- as.character(x)
  names(y) <- names(x)
  listing.character(y, ...)
}

listing.default <- function(x, ...) {
  listing(c(unclass(x)), ...)
}

listing.list <- function(x, ...) {
  listing(unlist(x), ...)
}

listing.character <- function(x, header = NULL, footer = NULL, prepend = FALSE,
    style = "list", collapse = if (style == "sentence")
      ""
    else
      "\n", force.numbers = FALSE,
    last.sep = c("and", "both", "comma", "or", "two"), hf.collapse = collapse,
    ...) {

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

  do_prepend <- function(x, prepend) paste0(spaces(L(prepend)), x)

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
      paste0(x, collapse = get_last_sep(last)),
      paste(paste0(x[-n], collapse = ", "), x[n], sep = get_last_sep(last))
    )
  }

  to_m4 <- function(x, single) {
    is_macro <- function(x) grepl("^[A-Za-z_]\\w*$", x, FALSE, TRUE)
    do_quote <- function(x, single) {
      x <- chartr("`", "'", x)
      if (single)
        sprintf("`%s'", gsub("'", "''`", x, FALSE, FALSE, TRUE))
      else
        sprintf("``%s''", gsub("'", "'''``", x, FALSE, FALSE, TRUE))
    }
    if (any(bad <- !is_macro(y <- names(x))))
      warning(sprintf("not a valid m4 macro string: '%s'", y[bad][1L]))
    sprintf("define(%s, %s)dnl", do_quote(y, TRUE), do_quote(x, single))
  }

  LL(style, collapse, force.numbers, hf.collapse)
  if ((is.null(names(x)) && style != "insert") || force.numbers)
    names(x) <- seq_along(x)
  if (inherits(style, "AsIs"))
    x <- structure(names(x), names = x)
  switch(style,
    table =,
    list = x <- do_prepend(formatDL(x = x, style = style, ...), prepend),
    sentence = x <- sentence(x, match.arg(last.sep), prepend),
    m4 = x <- to_m4(x, FALSE),
    M4 = x <- to_m4(x, TRUE),
    x <- do_prepend(sprintf(style, names(x), x), prepend)
  )
  if (identical(collapse, hf.collapse))
    paste0(c(header, x, footer), collapse = collapse)
  else
    paste0(c(header, paste0(x, collapse = collapse), footer),
      collapse = hf.collapse)
}

flatten <- function(object, ...) UseMethod("flatten")

flatten.default <- function(object, ...) {
  if (is.atomic(object))
    return(object)
  stop("need atomic 'object' (or specific 'flatten' method)")
}

flatten.list <- function(object, use.names = TRUE, ...) {
  while (any(is.a.list <- vapply(object, is.list, NA))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, FALSE, use.names)
  }
  object
}

collect <- function(x, what, ...) UseMethod("collect")

collect.list <- function(x,
    what = c("counts", "occurrences", "values", "elements", "datasets"),
    min.cov = 1L, keep.unnamed = FALSE, dataframe = FALSE, optional = TRUE,
    stringsAsFactors = default.stringsAsFactors(), ...) {

  # collecting 'counts' or 'occurrences'
  note_in_matrix <- function(x, count, min.cov, dataframe, optional,
      stringsAsFactors, ...) {
    x <- lapply(lapply(x, unlist), as.character)
    n <- sort.int(unique.default(unlist(x, FALSE, FALSE)))
    result <- matrix(0L, length(x), length(n), FALSE, list(names(x), n))
    if (count)
      for (i in seq_along(x)) {
        value <- table(x[[i]], useNA = "ifany")
        result[i, names(value)] <- unclass(value)
      }
    else
      for (i in seq_along(x))
        result[i, x[[i]]] <- 1L
    if (min.cov > 0L)
      result <- result[, colSums(result) >= min.cov, drop = FALSE]
    if (dataframe)
      as.data.frame(x = result, optional = optional,
        stringsAsFactors = stringsAsFactors, ...)
    else
      result
  }

  # collecting 'values' or 'elements'
  insert_into_matrix <- function(x, flat, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...) {
    oneify <- function(x) {
      if (is.atomic(x))
        return(x)
      size <- vapply(x, length, 0L)
      x[!size] <- NA
      x[size > 1L] <- lapply(x[size > 1L], list)
      x
    }
    keep_validly_named_only <- function(x) {
      if (is.null(names(x)))
        NULL
      else
        x[nzchar(names(x))]
    }
    keep_validly_named_only_but_complain <- function(x) {
      if (is.null(names(x))) {
        warning("removing unnamed vector")
        NULL
      } else if (!all(ok <- nzchar(names(x)))) {
        warning("removing elements with empty names")
        x[ok]
      } else
        x
    }
    enforce_names <- function(x) {
      names(x) <- if (is.null(n <- names(x)))
          seq_along(x)
        else
          ifelse(nzchar(n), n, seq_along(x))
      x
    }
    if (flat)
      x <- lapply(x, unlist)
    else
      x <- lapply(x, oneify)
    if (keep.unnamed)
      x <- lapply(x, enforce_names)
    else if (verbose)
      x <- lapply(x, keep_validly_named_only_but_complain)
    else
      x <- lapply(x, keep_validly_named_only)
    n <- unique.default(unlist(lapply(x, names), FALSE))
    result <- matrix(NA, length(x), length(n), FALSE, list(names(x), n))
    result <- as.data.frame(x = result, stringsAsFactors = FALSE,
      optional = TRUE, ...)
    for (i in seq_along(x))
      result[i, names(x[[i]])] <- x[[i]]
    if (dataframe && stringsAsFactors)
      for (i in which(vapply(result, is.character, NA)))
        result[, i] <- as.factor(result[, i])
    if (!optional)
      names(result) <- make.names(names(result))
    if (dataframe)
      result
    else
      as.matrix(result)
  }

  # collecting 'datasets'
  collect_matrices <- function(x, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...) {
    keep_validly_named_only <- function(x) {
      if (is.null(colnames(x)) || is.null(rownames(x)))
        NULL
      else
        x[nzchar(rownames(x)), nzchar(colnames(x)), drop = FALSE]
    }
    keep_validly_named_only_but_complain <- function(x) {
      if (is.null(colnames(x)) || is.null(rownames(x))) {
        warning("removing unnamed matrix or data frame")
        NULL
      } else {
        row.ok <- nzchar(rownames(x))
        col.ok <- nzchar(colnames(x))
        if (!all(row.ok) || !all(col.ok)) {
          warning("removing rows and/or columns with empty names")
          x[row.ok, col.ok, drop = FALSE]
        } else
          x
      }
    }
    enforce_names <- function(x) {
      enforce <- function(x, n) if (is.null(x))
          seq_len(n)
        else
          ifelse(nzchar(x), x, seq_len(n))
      rownames(x) <- enforce(rownames(x), nrow(x))
      colnames(x) <- enforce(colnames(x), ncol(x))
      x
    }
    if (all.atomic <- all(vapply(x, is.atomic, NA))) {
      x <- lapply(x, as.matrix)
      if (keep.unnamed)
        x <- lapply(x, enforce_names)
      else if (verbose)
        x <- lapply(x, keep_validly_named_only_but_complain)
      else
        x <- lapply(x, keep_validly_named_only)
    } else
      x <- lapply(x, data.frame, stringsAsFactors = FALSE,
        check.names = !optional)
    rn <- sort.int(unique.default(unlist(lapply(x, rownames))))
    cn <- sort.int(unique.default(unlist(lapply(x, colnames))))
    result <- matrix(NA, length(rn), length(cn), FALSE, list(rn, cn))
    if (!all.atomic)
      result <- as.data.frame(result, stringsAsFactors = FALSE,
        optional = optional)
    for (mat in x)
      result[rownames(mat), colnames(mat)] <- mat
    if (dataframe) {
      if (all.atomic)
        result <- as.data.frame(result)
      if (stringsAsFactors)
        for (i in which(vapply(result, is.character, NA)))
          result[, i] <- as.factor(result[, i])
    } else if (!all.atomic)
      result <- as.matrix(result)
    if (!optional)
      colnames(result) <- make.names(colnames(result))
    result
  }

  LL(min.cov, dataframe, optional, stringsAsFactors, keep.unnamed)
  if (verbose <- is.na(keep.unnamed))
    keep.unnamed <- FALSE
  case(match.arg(what),
    counts = note_in_matrix(x, TRUE, min.cov, dataframe, optional,
      stringsAsFactors, ...),
    occurrences = note_in_matrix(x, FALSE, min.cov, dataframe, optional,
      stringsAsFactors, ...),
    elements = insert_into_matrix(x, TRUE, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...),
    values = insert_into_matrix(x, FALSE, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...),
    datasets = collect_matrices(x, keep.unnamed, dataframe, optional,
      stringsAsFactors, verbose, ...)
  )
}

prepare_class_names <- function(x) UseMethod("prepare_class_names")

prepare_class_names.character <- function(x) {
  x <- unique.default(c("character", x))
  if ("ANY" %in% x)
    "ANY"
  else
    x
}

setGeneric("map_values",
  function(object, mapping, ...) standardGeneric("map_values"))

setMethod("map_values", c("list", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(coerce <- names(mapping)))
      return(object)
    mapfun <- function(item) as(item, map_values(class(item), mapping))
  } else
    mapfun <- if (length(coerce) == 0L || all(coerce == "character"))
      function(item) map_values(item, mapping)
    else
      function(item) {
        result <- map_values(as.character(item), mapping)
        mostattributes(result) <- attributes(item)
        result
      }
  map_values(object, mapping = mapfun, coerce = coerce)
}, sealed = SEALED)

setMethod("map_values", c("list", "function"), function(object, mapping,
    coerce = character(), ...) {
  rapply(object = object, f = mapping, classes = prepare_class_names(coerce),
    how = "replace", ...)
}, sealed = SEALED)

setMethod("map_values", c("list", "NULL"), function(object, mapping,
    coerce = character()) {
  clean_recursively <- function(x) {
    if (!is.list(x))
      return(x)
    x <- lapply(x, clean_recursively)
    x[vapply(x, length, 0L) > 0L]
  }
  if (length(coerce))
    object <- rapply(object, as.character, prepare_class_names(coerce), NULL,
      "replace")
  clean_recursively(object)
}, sealed = SEALED)

setMethod("map_values", c("list", "missing"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    classes <- "ANY"
    mapfun <- class
  } else {
    classes <- prepare_class_names(coerce)
    mapfun <- as.character
  }
  map_values(rapply(object, mapfun, classes = classes))
}, sealed = SEALED)

setMethod("map_values", c("list", "expression"), function(object, mapping,
    coerce = parent.frame()) {
  e <- list2env(object, NULL, coerce)
  for (subexpr in mapping)
    eval(subexpr, e)
  e <- as.list(e) # return 'e' if the order of list elements doesn't matter
  novel <- setdiff(names(e), names(object))
  for (name in setdiff(names(object), names(e)))
    object[[name]] <- NULL
  object[novel] <- e[novel]
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "function"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, inherits, NA, coerce)))
    object[[i]] <- mapping(object[[i]], ...)
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(coerce <- names(mapping)))
      return(object)
    mapfun <- function(item) as(item, map_values(class(item), mapping))
  } else
    mapfun <- function(item) map_values(as.character(item), mapping)
  map_values(object, mapping = mapfun, coerce = coerce)
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "NULL"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, inherits, NA, coerce)))
    object[[i]] <- as.character(object[[i]])
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "missing"), function(object,
    coerce = character()) {
  if (isTRUE(coerce))
    result <- unlist(lapply(object, class))
  else {
    coerce <- prepare_class_names(coerce)
    if (!"ANY" %in% coerce)
      object <- object[, vapply(object, inherits, NA, coerce),
        drop = FALSE]
    result <- unlist(lapply(object, as.character))
  }
  map_values(result)
}, sealed = SEALED)

setMethod("map_values", c("array", "character"), function(object, mapping,
    coerce = TRUE) {
  if (isTRUE(coerce)) {
    storage.mode(object) <- map_values(storage.mode(object), mapping)
    object
  } else {
    coerce <- prepare_class_names(coerce)
    if (!identical("ANY", coerce) && !storage.mode(object) %in% coerce)
      stop("storage mode of 'object' not contained in 'coerce'")
    result <- map_values(as.character(object), mapping)
    attributes(result) <- attributes(object)
    result
  }
}, sealed = SEALED)

setMethod("map_values", c("array", "missing"), function(object, coerce = TRUE) {
  if (isTRUE(coerce))
    result <- storage.mode(object)
  else {
    coerce <- prepare_class_names(coerce)
    if (!identical("ANY", coerce) && !storage.mode(object) %in% coerce)
      stop("storage mode of 'object' not contained in 'coerce'")
    result <- as.character(object)
  }
  map_values(result)
}, sealed = SEALED)

setMethod("map_values", c("array", "function"), function(object, mapping, ...) {
  result <- mapping(as.vector(object), ...)
  mostattributes(result) <- c(attributes(result), attributes(object))
  result
}, sealed = SEALED)

setMethod("map_values", c("character", "function"), function(object, mapping,
    ...) {
  result <- mapping(object, ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("character", "character"), function(object, mapping) {
  mapped <- match(object, names(mapping))
  object[found] <- mapping[mapped[found <- !is.na(mapped)]]
  object
}, sealed = SEALED)

setMethod("map_values", c("character", "missing"), function(object) {
  object <- sort.int(unique.default(object))
  structure(object, names = object)
}, sealed = SEALED)

setMethod("map_values", c("character", "NULL"), function(object, mapping) {
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "function"), function(object, mapping,
    ...) {
  levels(object) <- map_values(levels(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "character"), function(object, mapping) {
  levels(object) <- map_values(levels(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "missing"), function(object) {
  map_values(levels(object))
}, sealed = SEALED)

setMethod("map_values", c("logical", "function"), function(object, mapping,
    ...) {
  result <- mapping(object, ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("logical", "vector"), function(object, mapping) {
  result <- ifelse(object, mapping[[3L]], mapping[[1L]])
  result[is.na(result)] <- mapping[[2L]]
  attributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("logical", "NULL"), function(object, mapping) {
  object
}, sealed = SEALED)

setMethod("map_values", c("logical", "missing"), function(object) {
  result <- object * 2L + 1L
  result[is.na(result)] <- 2L
  attributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("NULL", "function"), function(object, mapping, ...) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "character"), function(object, mapping) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "missing"), function(object, mapping) {
  map_values(character())
}, sealed = SEALED)

setGeneric("map_names",
  function(object, mapping, ...) standardGeneric("map_names"))

setMethod("map_names", c("list", "function"), function(object, mapping, ...) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping, ...)
      lapply(item, FUN = map_names_recursively)
    } else
      item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "character"), function(object, mapping) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping)
      lapply(item, FUN = map_names_recursively)
    } else
      item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "missing"), function(object) {
  get_names_recursively <- function(item) {
    if (is.list(item))
      c(names(item), unlist(lapply(item, FUN = get_names_recursively)))
    else
      character()
  }
  map_values(get_names_recursively(object))
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "function"), function(object, mapping,
    ...) {
  dimnames(object) <- map_values(dimnames(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "character"), function(object, mapping) {
  dimnames(object) <- map_values(dimnames(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "missing"), function(object) {
  map_values(dimnames(object))
}, sealed = SEALED)

setMethod("map_names", c("array", "function"), function(object, mapping, ...) {
  dimnames(object) <- map_values(dimnames(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("array", "character"), function(object, mapping) {
  dimnames(object) <- map_values(dimnames(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("array", "missing"), function(object) {
  map_values(dimnames(object))
}, sealed = SEALED)

setMethod("map_names", c("ANY", "function"), function(object, mapping, ...) {
  names(object) <- map_values(names(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("ANY", "character"), function(object, mapping) {
  names(object) <- map_values(names(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("ANY", "missing"), function(object) {
  map_values(names(object))
}, sealed = SEALED)

setGeneric("contains",
  function(object, other, ...) standardGeneric("contains"))

setMethod("contains", c("list", "list"), function(object, other,
    values = TRUE, exact = FALSE, ...) {
  query.keys <- names(other)
  if (length(query.keys) == 0L && length(other) > 0L)
    return(FALSE)
  found <- match(query.keys, names(object), incomparables = "")
  if (any(is.na(found)))
    return(FALSE)
  for (idx in seq_along(query.keys)) {
    query.subset <- other[[idx]]
    data.subset <- object[[found[idx]]]
    result <- if (is.list(query.subset)) {
      if (is.list(data.subset))
        Recall(object = data.subset, other = query.subset, values = values,
          exact = exact, ...)
      else if (values)
        FALSE
      else
        is.null(names(query.subset))
    } else if (values) {
      if (exact)
        identical(x = data.subset, y = query.subset, ...)
      else
        all(data.subset %in% query.subset)
    } else
      TRUE
    if (!result)
      return(FALSE)
  }
  TRUE
}, sealed = SEALED)

