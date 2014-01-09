setClass("DBTABLES",
  contains = "VIRTUAL",
  sealed = SEALED
)

NULL

print.DBTABLES_Summary <- function(x, ...) {
  cat("An object of class ", sQuote(x$Class), ".\n", sep = "")
  cat("Number of rows in first table: ", x$Size, "\n", sep = "")
  cat("Defined cross-references between tables:\n")
  cr <- x$Crossrefs
  cr[is.na(cr)] <- "<MISSING>"
  cr <- structure(paste(cr[, "to.tbl"], cr[, "to.col"], sep = "."),
    names = paste(cr[, "from.tbl"], cr[, "from.col"], sep = "."))
  cat(formatDL(cr), ..., sep = "\n")
  invisible(x)
}

NULL

setGeneric("fkeys", function(object) standardGeneric("fkeys"))

setMethod("fkeys", "DBTABLES", function(object) {
  pk <- pkeys(object)
  do.call(rbind, lapply(names(pk), function(n) {
      refs <- grep("^\\w+_id$", colnames(slot(object, n)), FALSE, TRUE, TRUE)
      if (length(refs))
        to.tbl <- paste0(substr(refs, 1L, nchar(refs) - 3L), "s")
      else
        refs <- to.tbl <- NA_character_
      cbind(from.tbl = n, from.col = refs, to.tbl = to.tbl, to.col = pk[[n]])
    }))
}, sealed = SEALED)

setGeneric("pkeys", function(object) standardGeneric("pkeys"))

setMethod("pkeys", "DBTABLES", function(object) {
  x <- slotNames(object)
  structure(rep.int("id", length(x)), names = x)
}, sealed = SEALED)

setGeneric("fkeys_valid",
  function(object) standardGeneric("fkeys_valid"))

setMethod("fkeys_valid", "DBTABLES", function(object) {
  join <- function(x) paste0(unique.default(x), collapse = " ")
  x <- fkeys(object)[-1L, , drop = FALSE]
  bad <- is.na(x[, "from.col"])
  errs <- sprintf("no references in slot '%s'", x[bad, "from.tbl"])
  x <- x[!bad, , drop = FALSE]
  bad <- apply(x, 1L, function(row) tryCatch({
      other <- slot(object, row["to.tbl"])[, row["to.col"]]
      self <- slot(object, row["from.tbl"])[, row["from.col"]]
      if (!all(self %in% other))
        stop(sprintf("dead references: %s <=> %s", join(self), join(other)))
      if (!all(other %in% self))
        stop(sprintf("superfluous ids: %s <=> %s", join(other), join(self)))
      NA_character_
    }, error = conditionMessage))
  if (any(really <- !is.na(bad)))
    errs <- c(errs, sprintf("problem in %s/%s: %s",
      x[really, "from.tbl"], x[really, "to.tbl"], bad[really]))
  if (length(errs))
    errs
  else
    TRUE
}, sealed = SEALED)

setGeneric("pkeys_valid",
  function(object) standardGeneric("pkeys_valid"))

setMethod("pkeys_valid", "DBTABLES", function(object) {
  pk <- pkeys(object)
  result <- mapply(function(slotname, colname) tryCatch({
      if (anyDuplicated.default(slot(object, slotname)[, colname]))
        stop("non-unique IDs")
      NA_character_
    }, error = conditionMessage), names(pk), pk)
  if (length(result <- result[!is.na(result)]))
    sprintf("problem in %s: %s", names(result), result)
  else
    TRUE
}, sealed = SEALED)

setGeneric("summary")

setMethod("summary", "DBTABLES", function(object) {
  structure(list(Class = class(object), Size = length(object),
    Crossrefs = fkeys(object)), class = "DBTABLES_Summary")
}, sealed = SEALED)

setMethod("show", "DBTABLES", function(object) {
  print(summary(object))
}, sealed = SEALED)

setMethod("length", "DBTABLES", function(x) {
  nrow(slot(x, names(pkeys(x))[1L]))
}, sealed = SEALED)

setGeneric("head")

setMethod("head", "DBTABLES", function(x) {
  pk <- pkeys(x)
  mapply(function(slotname, colname) min(slot(x, slotname)[, colname]),
    names(pk), pk)
}, sealed = SEALED)

setGeneric("tail")

setMethod("tail", "DBTABLES", function(x) {
  pk <- pkeys(x)
  mapply(function(slotname, colname) max(slot(x, slotname)[, colname]),
    names(pk), pk)
}, sealed = SEALED)

setGeneric("sort")

setMethod("sort", c("DBTABLES", "missing"), function(x, decreasing) {
  sort(x, FALSE)
}, sealed = SEALED)

setMethod("sort", c("DBTABLES", "logical"), function(x, decreasing) {
  sort_by_id <- function(x, idx) x[sort.list(x[, idx], NULL, TRUE,
    decreasing), , drop = FALSE]
  for (i in seq_along(pk <- pkeys(x)))
    slot(x, tn) <- sort_by_id(slot(x, tn <- names(pk)[i]), pk[i])
  x
}, sealed = SEALED)

setGeneric("update")

setMethod("update", "DBTABLES", function(object, start, drop = TRUE) {
  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }
  reset <- function(x, where, start) {
    if (add <- start - min(x[, where]))
      x[, where] <- x[, where] + add
    x
  }
  pk <- pkeys(object)
  if (is.null(start)) {
    start <- rep.int(1L, length(pk))
    names(start) <- names(pk)
  } else {
    if (any(is.na(start)))
      stop("'start' contains missing values")
    if (is.null(names(start)))
      names(start) <- names(pk)[seq_along(start)]
  }
  if (drop)
    for (tn in names(pk))
      slot(object, tn) <- unrowname(slot(object, tn))
  storage.mode(start) <- "integer"
  if (!length(start <- start[!!start]))
    return(object)
  crs <- fkeys(object)
  crs <- crs[!is.na(crs[, "to.tbl"]), , drop = FALSE]
  for (i in seq_along(start)) {
    tn <- names(start)[i]
    slot(object, tn) <- reset(slot(object, tn), pk[[tn]], start[i])
    cr <- crs[crs[, "to.tbl"] == tn, , drop = FALSE]
    for (j in seq_len(nrow(cr))) {
      tn <- cr[j, "from.tbl"]
      slot(object, tn) <- reset(slot(object, tn), cr[j, "from.col"], start[i])
    }
  }
  object
}, sealed = SEALED)

setMethod("c", "DBTABLES", function(x, ..., recursive = FALSE) {
  if (recursive)
    x <- update(x, NULL, TRUE)
  if (missing(..1))
    return(x)
  klass <- class(x)
  pk <- pkeys(x)
  x <- list(x, ...)
  for (i in seq_along(x)[-1L])
    x[[i]] <- update(x[[i]], tail(x[[i - 1L]]) + 1L, recursive)
  result <- sapply(X = names(pk), simplify = FALSE,
    FUN = function(slotname) do.call(rbind, lapply(x, slot, slotname)))
  do.call(new, c(list(Class = klass), result))
}, sealed = SEALED)

setGeneric("split")

setMethod("split", c("DBTABLES", "missing", "missing"), function(x, f, drop) {
  f <- pkeys(x)[1L]
  split(x, slot(x, names(f))[, f], TRUE)
}, sealed = SEALED)

setMethod("split", c("DBTABLES", "missing", "logical"), function(x, f, drop) {
  f <- pkeys(x)[1L]
  split(x, slot(x, names(f))[, f], drop)
}, sealed = SEALED)

setMethod("split", c("DBTABLES", "ANY", "missing"), function(x, f, drop) {
  split(x, f, TRUE)
}, sealed = SEALED)

setMethod("split", c("DBTABLES", "ANY", "logical"), function(x, f, drop) {
  id2pos <- function(x, i) structure(rep.int(i, length(x)), names = x)
  get_mapping <- function(x, key, text) {
    if (!is.list(x))
      stop("ordering problem encountered ", paste0(text, collapse = "->"))
    x <- lapply(lapply(x, `[[`, key), unique.default)
    x <- mapply(id2pos, x, seq_along(x), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    unlist(x, FALSE, TRUE)
  }
  class.arg <- list(Class = class(x))
  result <- as.list(pkeys(x))
  result[[1L]] <- split(slot(x, names(result)[1L]), f, TRUE)
  crs <- fkeys(x)
  for (i in seq_along(result)[-1L]) {
    this <- names(result)[i]
    cr <- crs[crs[, "from.tbl"] == this, , drop = FALSE][1L, ]
    mapping <- get_mapping(result[[cr[["to.tbl"]]]], cr[["to.col"]], cr)
    this <- slot(x, this)
    grps <- mapping[as.character(this[, cr[["from.col"]]])]
    result[[i]] <- split(this, grps, TRUE)
  }
  result <- lapply(seq_along(result[[1L]]), function(i) lapply(result, `[[`, i))
  result <- lapply(result, function(x) do.call(new, c(class.arg, x)))
  if (drop)
    result <- lapply(result, update, NULL, TRUE)
  result
}, sealed = SEALED)

setGeneric("by")

setMethod("by", c("DBTABLES", "ANY", "character", "missing"), function(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) {
  by(data, INDICES, match.fun(FUN), ..., do_map = do_map, do_inline = do_inline,
    do_quote = do_quote, simplify = do_inline)
}, sealed = SEALED)

setMethod("by", c("DBTABLES", "ANY", "character", "logical"), function(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) {
  by(data, INDICES, match.fun(FUN), ..., do_map = do_map, do_inline = do_inline,
    do_quote = do_quote, simplify = simplify)
}, sealed = SEALED)

setMethod("by", c("DBTABLES", "ANY", "function", "missing"), function(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) {
  by(data, INDICES, FUN, ..., do_map = do_map, do_inline = do_inline,
    do_quote = do_quote, simplify = do_inline)
}, sealed = SEALED)

setMethod("by", c("DBTABLES", "ANY", "function", "logical"), function(data,
    INDICES, FUN, ..., do_map = NULL, do_inline = FALSE, do_quote = '"',
    simplify) {

  map_items <- function(x, mapping) {
    if (!length(names(mapping)))
      return(x)
    pos <- match(x, names(mapping), 0L)
    x[found] <- mapping[pos[found <- pos > 0L]]
    x
  }

  sq <- function(x) if (is.numeric(x))
    x
  else
    sprintf("'%s'", gsub("'", "''", x, FALSE, FALSE, TRUE))

  if (is.function(do_quote))
    dq <- do_quote
  else
    dq <- function(x) sprintf(sprintf("%s%%s%s", do_quote, do_quote),
      gsub(do_quote, paste0(do_quote, do_quote), x, FALSE, FALSE, TRUE))

  if (do_inline) {

    get_fun <- if (simplify)
        function(.TBL, .COL, .IDX, ...) FUN(
          sprintf("SELECT * FROM %s WHERE %s;", dq(.TBL),
          paste(dq(.COL), sq(.IDX), sep = " = ", collapse = " OR ")), ...)
      else
        FUN
    pk <- pkeys(data)
    result <- lapply(as.list(pk), as.null)
    result[[1L]] <- as.data.frame(get_fun(
      map_items(names(result)[1L], do_map), pk[1L], INDICES, ...))
    crs <- fkeys(data)
    crs <- crs[!is.na(crs[, "to.tbl"]), , drop = FALSE]
    for (i in seq_len(nrow(crs))) {
      cr <- crs[i, ]
      if (!is.null(result[[cr[["from.tbl"]]]]))
        next
      INDICES <- result[[cr[["to.tbl"]]]][, cr[["to.col"]]]
      result[[cr[["from.tbl"]]]] <- as.data.frame(get_fun(map_items(
        cr[["from.tbl"]], do_map), cr[["from.col"]], INDICES, ...))
    }
    do.call(new, c(list(Class = class(data)), result))

  } else if (simplify) {

    ids <- pkeys(data)[INDICES]
    tns <- map_items(names(ids), do_map)
    mapply(FUN = FUN, tns, ids, MoreArgs = list(...))

  } else {

    tn2 <- map_items(tn1 <- names(pkeys(data))[INDICES], do_map)
    mapply(FUN = FUN, tn2, lapply(tn1, slot, object = data),
      MoreArgs = list(...))

  }

}, sealed = SEALED)

