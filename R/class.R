class_desc <- function(x) UseMethod("class_desc")

class_desc.list <- function(x) {
  if (is.null(names(x)))
    stop("list 'x' must have names")
  if (!all(vapply(x, inherits, NA, "character")))
    stop("list 'x' must contain only character vectors")
  class(x) <- "class_desc"
  x
}

class_desc.class_desc <- function(x) x

classes_desc <- function(x) UseMethod("classes_desc")

classes_desc.list <- function(x) {
  if (is.null(names(x)))
    stop("list 'x' must have names")
  x <- lapply(x, class_desc)
  if (!all(names(x) == vapply(x, class_name, "")))
    stop("inconsistencies between list names and class-name entries")
  class(x) <- "classes_desc"
  x
}

classes_desc.classes_desc <- function(x) x

class_rdfiles <- function(pkg, ...) UseMethod("class_rdfiles")

class_rdfiles.character <- function(pkg, classes,
    what = c("dump", "update", "name", "content"), ...) {
  default_rdfile_name <- function(pkg, classes) {
    file.path(pkg, "man", paste(classes, "class.Rd", sep = "-"))
  }
  get_package <- function(pkg) {
    if (file.exists(pkg)) {
      pack_desc(pkg, "source")
      FALSE
    } else if (pkg %in% .packages())
      FALSE
    else if (require(pkg, quietly = TRUE, warn.conflicts = FALSE,
        character.only = TRUE))
      TRUE
    else
      stop(sprintf("package '%s' can neither be source'd nor require'd", pkg))
  }
  must.detach <- FALSE
  on.exit(if (must.detach)
    detach(sprintf("package:%s", pkg), character.only = TRUE)
  )
  case(match.arg(what),
    name = default_rdfile_name(pkg, classes),
    dump = {
      must.detach <- get_package(pkg)
      invisible(mapply(promptClass, classes,
        default_rdfile_name(pkg, classes)))
    },
    update = {
      must.detach <- get_package(pkg)
      cd <- classes_desc(sapply(classes, promptClass, filename = NA,
        simplify = FALSE))
      cd <- update(object = cd, ...)
      puts(cd, default_rdfile_name(pkg, classes))
    },
    content = {
      must.detach <- get_package(pkg)
      classes_desc(sapply(classes, promptClass, filename = NA,
        simplify = FALSE))
    }
  )
}

class_name <- function(x) UseMethod("class_name")

class_name.class_desc <- function(x) {
  x <- sub("\\name{", "", x$name, FALSE, FALSE, TRUE)
  sub("-class}", "", x, FALSE, FALSE, TRUE)
}

class_name.classes_desc <- function(x) {
  names(x)
}

NULL

update.classes_desc <- function(object, ...) {
  structure(lapply(X = object, FUN = update, ...), class = oldClass(object))
}

update.class_desc <- function(object,
    outcomment = c("note", "author", "references", "seealso"),
    description = sprintf("  See %s.",
      rd_quote(c("code", "link"), class_name(object))),
    ...) {
  rd_comment <- function(x) {
    sprintf("%% %s", gsub("([\n\r]+)", "\\1%% ", x, FALSE, TRUE))
  }
  object[outcomment] <- lapply(object[outcomment], rd_comment)
  others <- list(description = description, ...)
  others <- others[names(others) %in% names(object)]
  others <- others[!names(others) %in% outcomment]
  others <- others[vapply(others, length, 0L) > 0L]
  for (name in names(others))
    object[[name]] <- as.character(others[[name]])
  object
}

update.pack_desc <- function(object, version = TRUE, date.format = "%Y-%m-%d",
    ...) {
  LL(version, date.format)
  if (is.null(old.date <- object$Date)) {
    warning(sprintf("file '%s' contains no date", attr(object, "file")))
    object$Date <- format(Sys.time(), date.format)
  } else {
    object$Date <- format(Sys.time(), date.format)
    if (version && old.date < object$Date)
      object$Version <- as.character(update(numeric_version(object$Version)))
  }
  object
}

update.pack_descs <- function(object, ...) {
  structure(lapply(X = object, FUN = update, ...), class = oldClass(object))
}

update.numeric_version <- function(object, ...) {
  incr <- function(x) {
    if (n <- length(x)) # invalid version strings yielded zero-length vectors
      x[n] <- x[n] + 1L
    x
  }
  object[] <- rapply(object, incr, classes = "integer", how = "replace")
  object
}

