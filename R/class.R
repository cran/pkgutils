

################################################################################

#' Create class-description objects
#'
#' Constructor functions for class-description objects. These are used for
#' defining specific methods for the result of \code{promptClass} from the
#' \pkg{methods} package.
#'
#' @param x \R object to be converted.
#~ @export
#' @return Object of class \sQuote{class_desc} or \sQuote{classes_desc}.
#' @note  None of these objects is currently used by the \sQuote{docu.R}
#'   script that comes with the package.
#' @seealso methods::promptClass
#~ @family class-functions
#~ @keywords methods
#' @keywords internal
#~ @examples
#~ # See class_rdfiles() for an example in which a 'classes_desc' object is
#~ # returned.
#'
class_desc <- function(x) UseMethod("class_desc")

#' @rdname class_desc
#' @method class_desc list
#' @export
#'
class_desc.list <- function(x) {
  if (is.null(names(x)))
    stop("list 'x' must have names")
  if (!all(vapply(x, inherits, logical(1L), what = "character")))
    stop("list 'x' must contain only character vectors")
  class(x) <- "class_desc"
  x
}

#' @rdname class_desc
#' @method class_desc class_desc
#' @export
#'
class_desc.class_desc <- function(x) x

#' @rdname class_desc
#~ @export
#'
classes_desc <- function(x) UseMethod("classes_desc")

#' @rdname class_desc
#' @method classes_desc list
#' @export
#'
classes_desc.list <- function(x) {
  if (is.null(names(x)))
    stop("list 'x' must have names")
  x <- lapply(x, class_desc)
  if (!all(names(x) == vapply(x, class_name, character(1L))))
    stop("inconsistencies between list names and class-name entries")
  class(x) <- "classes_desc"
  x
}

#' @rdname class_desc
#' @method classes_desc classes_desc
#' @export
#'
classes_desc.classes_desc <- function(x) x


################################################################################


#' Dump or name class RD files
#'
#' Create RD files for the classes belonging to a given package. This is based
#' on \code{promptClass} from the \pkg{methods} package. Alternatively, just
#' return the names of the class.
#'
#' @inheritParams pack_desc
#' @param classes Character vector with the names of classes of interest.
#' @param what Character scalar determing the main mode of action.
#' @param ... Optional arguments passed to \code{\link{update}}. Ignored unless
#'   \code{what} is \sQuote{update}.
#~ @export
#' @return List of \code{promptClass} results, returned invisibly.
#' @seealso methods::promptClass
#~ @family class-functions
#~ @keywords methods
#' @keywords internal
#~ @examples
#~
#~ # just the file names
#~ (x <- class_rdfiles("foo", c("workingclass", "aclassofitsown"), "name"))
#~ stopifnot(is.character(x), length(x) == 2, grepl("\\.Rd$", x))
#~
#~ # content returned
#~ (x <- class_rdfiles("methods", "ObjectsWithPackage", "content"))
#~ stopifnot(is.list(x), names(x) == "ObjectsWithPackage")
#~ stopifnot(inherits(x, "classes_desc"))
#'
class_rdfiles <- function(pkg, ...) UseMethod("class_rdfiles")

#' @rdname class_rdfiles
#' @method class_rdfiles character
#' @export
#'
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


################################################################################


#' Find class names
#'
#' Find the name(s) of the documented class(es) within a \sQuote{class_desc}
#' or \sQuote{classes_desc} object.
#'
#' @param x Object of class \sQuote{class_desc} or \sQuote{classes_desc}.
#' @return Character vector.
#~ @export
#~ @family class-functions
#~ @keywords methods
#' @keywords internal
#~ @examples
#~ x <- class_rdfiles("methods", "ObjectsWithPackage", "content")
#~ (y <- class_name(x))
#~ stopifnot(y == "ObjectsWithPackage")
#'
class_name <- function(x) UseMethod("class_name")

#' @rdname class_name
#' @method class_name class_desc
#' @export
#'
class_name.class_desc <- function(x) {
  sub("-class}", "", sub("\\name{", "", x$name, fixed = TRUE), fixed = TRUE)
}

#' @rdname class_name
#' @method class_name classes_desc
#' @export
#'
class_name.classes_desc <- function(x) {
  names(x)
}


################################################################################


#' Modify class- or package-description objects
#'
#' Modify class-description objects by outcommenting and otherwise changing
#' sections, or modify package-description objects by updating the date and
#' optionally also the version.
#'
#' @inheritParams pack_desc
#' @param object Object of class \sQuote{class_desc}, \sQuote{classes_desc},
#'   \sQuote{pack_desc} or \sQuote{pack_descs}.
#' @param outcomment Character vector with the names of entries to be commented
#'   out.
#' @param description A replacement for the \sQuote{description} entry of the
#'   class. Ignored if empty or if \sQuote{description} occurs in
#'   \sQuote{outcomment}.
#' @param ... For the \sQuote{class_desc} and \sQuote{classes_desc} methods,
#'   optional other named arguments to be set. Ignored if the name does
#'   not occur in the names of \code{object} or if it does occur in
#'   \code{outcomment} or if the value has zero length.
#~ @export
#' @return \R object of the same class than \code{object}.
#' @name update
#~ @family class-functions
#~ @keywords manip
#' @keywords internal
#~ @examples
#~
#~ # 'classes_desc' objects
#~ x <- class_rdfiles("methods", "ObjectsWithPackage", "content")
#~ (y <- update(x, description = NULL))
#~ stopifnot(class(x) == class(y), !identical(x, y))
#~ stopifnot(sapply(y, length) == sapply(x, length))
#~ stopifnot(sapply(y, nchar) >= sapply(x, nchar))
#~ stopifnot(!all(sapply(y, nchar) == sapply(x, nchar)))
#~ (y <- update(x, description = "short"))
#~ stopifnot(!all(sapply(y, nchar) >= sapply(x, nchar)))
#~
#~ # see pack_desc for 'pack_desc' objects
#'
NULL

#' @rdname update
#' @method update classes_desc
#' @export
#'
update.classes_desc <- function(object, ...) {
  structure(lapply(X = object, FUN = update, ...), class = oldClass(object))
}

#' @rdname update
#' @method update class_desc
#' @export
#'
update.class_desc <- function(object,
    outcomment = c("note", "author", "references", "seealso"),
    description = sprintf("  See %s.",
      rd_quote(c("code", "link"), class_name(object))),
    ...) {
  rd_comment <- function(x) {
    sprintf("%% %s", gsub("([\n\r]+)", "\\1%% ", x, perl = TRUE))
  }
  object[outcomment] <- lapply(object[outcomment], rd_comment)
  others <- list(description = description, ...)
  others <- others[names(others) %in% names(object)]
  others <- others[!names(others) %in% outcomment]
  others <- others[vapply(others, length, integer(1L)) > 0L]
  for (name in names(others))
    object[[name]] <- as.character(others[[name]])
  object
}

#' @rdname update
#' @method update pack_desc
#' @export
#'
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

#' @rdname update
#' @aliases update.pack_descs
#' @method update pack_descs
#' @export
#'
update.pack_descs <- function(object, ...) {
  structure(lapply(X = object, FUN = update, ...), class = oldClass(object))
}

#' @rdname update
#' @method update numeric_version
#' @export
#'
update.numeric_version <- function(object, ...) {
  incr <- function(x) {
    if (n <- length(x)) # invalid version strings yielded zero-length vectors
      x[n] <- x[n] + 1L
    x
  }
  object[] <- rapply(object, incr, classes = "integer", how = "replace")
  object
}


################################################################################


