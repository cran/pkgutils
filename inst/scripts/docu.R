#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# docu.R -- Rscript script to non-interactively create documentation for an R
#   package with Roxygen2, to improve this documentation, to update the 
#   DESCRIPTION file, to check the package and to install the package. Can also
#   check some aspects of the R coding style and conduct some preprocessing of
#   the code files. Part of the pkgutils package. See the manual for details.
#
# Package directory names can be provided at the command line. If none are
# given, the working directory is checked for subdirectories that contain a 
# DESCRIPTION file, and it is then attempted to document those, if any.
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


for (lib in c("methods", "pkgutils", "roxygen2", "optparse"))
  library(lib, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)


################################################################################


do_check <- function(files, opt) {
  y <- pkgutils::check_R_code(x = files, lwd = opt$width, ops = !opt$opsoff,
    comma = !opt$commaoff, indention = opt$blank, roxygen.space = opt$jspaces,
    modify = opt$modify, ignore = opt$good, parens = !opt$parensoff,
    assign = !opt$assignoff)
  if (any(y))
    message(paste(sprintf("file '%s' has been modified", names(y)[y]),
      collapse = "\n"))
  y
}


do_split <- function(x, sep = ",") {
  x <- unlist(strsplit(x, sep, fixed = TRUE))
  x[nzchar(x)]
}


################################################################################


option.parser <- OptionParser(option_list = list(

  make_option(c("-a", "--ancient"), action = "store_true", default = FALSE,
    help = "Do not update the DESCRIPTION file [default: %default]"),

  make_option(c("-A", "--assignoff"), action = "store_true", default = FALSE,
    help = "Ignore assignments when checking R style [default: %default]"),

  make_option(c("-b", "--blanks"), type = "integer", default = 2L,
    help = "Number of spaces per indention unit in R code [default: %default]",
    metavar = "NUMBER"),

  # B

  make_option(c("-c", "--check"), action = "store_true", default = FALSE,
    help = "Run 'R CMD check' after documenting [default: %default]"),

  make_option(c("-C", "--commaoff"), action = "store_true", default = FALSE,
    help = "Ignore comma treatment when checking R style [default: %default]"),

  make_option(c("-d", "--delete"), type = "character", default = "",
    help = "Subdirectories to delete, colon-separated list [default: %default]",
    metavar = "LIST"),

  # D

  make_option(c("-e", "--exec"), type = "character", default = "ruby",
    help = "Ruby executable used if -p or -s is chosen [default: %default]",
    metavar = "FILE"),

  # E

  make_option(c("-f", "--format"), type = "character", default = "%Y-%m-%d",
    help = "Format of the date in the DESCRIPTION file [default: %default]",
    metavar = "STR"),

  # F

  # A bug in Rscript causes '-g' to generate strange warning messages.
  # See https://stat.ethz.ch/pipermail/r-devel/2008-January/047944.html
  make_option(c("-g", "--good"), type = "character", default = "",
    help = paste("R code files to not check, comma-separated list",
    "[default: %default]"), metavar = "LIST"),

  # G

  # h is reserved for help!

  # H

  make_option(c("-i", "--install"), action = "store_true", default = FALSE,
    help = "Also run 'R CMD INSTALL' after documenting [default: %default]"),

  # I

  make_option(c("-j", "--jspaces"), type = "integer", default = 1L,
    help = paste("Number of spaces starting Roxygen-style comments",
    "[default: %default]"), metavar = "NUMBER"),

  # J

  make_option(c("-k", "--keep"), action = "store_true", default = FALSE,
    help = "Keep the version number in DESCRIPTION files [default: %default]"),

  # K

  make_option(c("-l", "--logfile"), type = "character", default = "docu_%s.log",
    help = "Logfile to use for problem messages [default: %default]",
    metavar = "FILE"),

  # L

  make_option(c("-m", "--modify"), action = "store_true", default = FALSE,
    help = paste("Potentially modify R sources when style checking",
    "[default: %default]")),

  # M

  make_option(c("-n", "--nosudo"), action = "store_true", default = FALSE,
    help = "In conjunction with -i, do not use sudo [default: %default]"),

  # N

  make_option(c("-o", "--options"), type = "character", default = "as-cran",
    help = "'R CMD check' options, comma-separated list [default: %default]",
    metavar = "LIST"),

  make_option(c("-O", "--opsoff"), action = "store_true", default = FALSE,
    help = "Ignore operators when checking R style [default: %default]"),

  make_option(c("-p", "--preprocess"), action = "store_true", default = FALSE,
    help = "Preprocess R files using code swapping [default: %default]"),

  make_option(c("-P", "--parensoff"), action = "store_true", default = FALSE,
    help = "Ignore parentheses when checking R style [default: %default]"),

  make_option(c("-q", "--quick"), action = "store_true", default = FALSE,
    help = "Do not remove duplicate 'seealso' links [default: %default]"),

  # Q

  make_option(c("-r", "--remove"), action = "store_true", default = FALSE,
    help = "First remove output directories if distinct [default: %default]"),

  make_option(c("-R", "--Rcheck"), action = "store_true", default = FALSE,
    help = "Check only format of R code, ignore packages [default: %default]"),

  make_option(c("-s", "--s4methods"), action = "store_true", default = FALSE,
    help = "Repair S4 method descriptions [default: %default]"),

  # S

  make_option(c("-t", "--target"), type = "character",
    default = file.path(Sys.getenv("HOME"), "bin"),
    help = paste("For -i, script file target directory; ignored if",
      "empty [default: %default]"), metavar = "DIR"),

  # T

  make_option(c("-u", "--unsafe"), action = "store_true", default = FALSE,
    help = "In conjunction with -i, omit checking [default: %default]"),

  make_option(c("-U", "--untidy"), action = "store_true", default = FALSE,
    help = "Omit R style checking altogether [default: %default]"),

  make_option(c("-v", "--verbatim"), action = "store_true", default = FALSE,
    help = paste("No special processing of package names ending in '*_in'",
      "[default: %default]")),

  # V

  make_option(c("-w", "--width"), type = "integer", default = 80L,
    help = "Maximum allowed line width in R code [default: %default]",
    metavar = "NUMBER"),

  # W

  make_option(c("-x", "--exclude"), type = "character", default = "",
    help = paste("Files to ignore when using -t, comma-separated list",
      "[default: %default]"), metavar = "LIST"),

  # X

  make_option(c("-y", "--yes"), action = "store_true", default = FALSE,
    help = "Yes, also build the package [default: %default]"),

  # Y

  make_option(c("-z", "--zapoff"), action = "store_true", default = FALSE,
    help = "Do not zap object files in 'src', if any [default: %default]")

  # Z

), usage = "%prog [options] [directories/files]")


opt <- parse_args(option.parser, positional_arguments = TRUE)
package.dirs <- opt$args
opt <- opt$options


if (opt$help || (opt$Rcheck && !length(package.dirs))) {
  print_help(option.parser)
  quit(status = 1L)
}


opt$options <- do_split(opt$options)
opt$delete <- do_split(opt$delete, ":")
opt$good <- basename(do_split(opt$good))
opt$exclude <- do_split(opt$exclude)


################################################################################


if (opt$Rcheck) {
  quit(status = length(which(is.na(do_check(package.dirs, opt)))))
}


################################################################################



if (length(package.dirs)) {
  if (length(bad <- package.dirs[!pkgutils::is_pkg_dir(package.dirs)]))
    stop("not a package directory: ", bad[1L])
} else {
  package.dirs <- list.files()
  package.dirs <- package.dirs[pkgutils::is_pkg_dir(package.dirs)]
  if (!length(package.dirs)) {
    warning("no package directories provided, and none found in ", getwd(),
      "\nuse command-line switches '--help' or '-h' for help")
    quit(status = 1L)
  }
}

if (opt$verbatim) {
  out.dirs <- package.dirs
} else {
  out.dirs <- sub("_in$", "", package.dirs, perl = TRUE)
  ok <- package.dirs != out.dirs
  ok[!ok][!package.dirs[!ok] %in% out.dirs[ok]] <- TRUE
  package.dirs <- package.dirs[ok]
  out.dirs <- out.dirs[ok]
  rm(ok)
  if (opt$remove) {
    message("Removing distinct output directories (if any)...")
    unlink(out.dirs[out.dirs != package.dirs], recursive = TRUE)
  }
}

msgs <- sprintf(" package directory '%s'...", out.dirs)

logfiles <- if (nzchar(opt$logfile)) {
  file.path(dirname(out.dirs), sprintf(opt$logfile, basename(out.dirs)))
} else {
  rep("", length(out.dirs))
}


errs <- 0L


################################################################################


if (nzchar(opt$target) && !utils::file_test("-d", opt$target)) {
  warning(sprintf("'%s' is not a directory", opt$target))
  opt$target <- ""
  errs <- errs + 1L
}


for (i in seq_along(package.dirs)) {

  out.dir <- out.dirs[i]
  in.dir <- package.dirs[i]
  msg <- msgs[i]

  if (!identical(out.dir, in.dir)) {
    # Copying the files is preferred to calling roxygenize() with two
    # directory arguments because due to a Roxygen2 bug this would result in
    # duplicated documentation for certain S4 methods.
    message(sprintf("Copying '%s' to '%s'...", in.dir, out.dir))
    unlink(out.dir, recursive = TRUE)
    dir.create(out.dir) &&
      file.copy(list.files(in.dir, recursive = FALSE, full.names = TRUE),
        out.dir, recursive = TRUE)
    if (length(opt$delete)) {
      message("Deleting specified subdirectories (if present) of", msg)
      unlink(file.path(out.dir, opt$delete), recursive = TRUE)
    }
  }

  message(sprintf("Logfile is now '%s'", pkgutils::logfile(logfiles[i])))

  if (!opt$untidy) {
    message("Checking R code of", msg)
    errs <- errs + length(which(is.na(do_check(out.dir, opt))))
  }

  message("Creating documentation for", msg)
  roxygen2::roxygenize(out.dir)

  message("Repairing documentation for", msg)
  pkgutils::repair_docu(out.dir, remove.dups = !opt$quick)

  if (opt$s4methods) {
    message("Repairing S4 method documentation for", msg)
    errs <- errs + pkgutils::repair_S4_docu(out.dir, ruby = opt$exec)
  }

  if (suppressWarnings(file.remove(file.path(out.dir, "inst"))))
    message("Deleting empty 'inst' subdirectory of", msg)

  if (!opt$ancient) {
    message("Updating DESCRIPTION of", msg)
    tmp <- pkgutils::pack_desc(out.dir, "update", version = !opt$keep,
      date.format = opt$format)
    message(paste(formatDL(tmp[1L, ], style = "list"), collapse = "\n"))
  }

  if (opt$preprocess) {
    message("Preprocessing R code of", msg)
    errs <- errs + pkgutils::swap_code(out.dir, ruby = opt$exec)
  }

  if (opt$check || ((opt$install || opt$yes) && !opt$unsafe)) {
    if (!opt$zapoff) {
      message("Deleting object files (if any) of", msg)
      errs <- errs + !all(delete_o_files(out.dir))
    }
    message("Checking", msg)
    errs <- errs + (check.result <- run_R_CMD(out.dir, "check", opt$options))
  }

  if (opt$yes && (opt$unsafe || identical(check.result, 0L))) {
    message("Building", msg)
    errs <- errs + run_R_CMD(out.dir, "build")
  }

  if (opt$install && (opt$unsafe || identical(check.result, 0L))) {
    message("Installing", msg)
    errs <- errs +
      (installed <- run_R_CMD(out.dir, "INSTALL", sudo = !opt$nosudo))
    if (identical(installed, 0L) && nzchar(opt$target)) {
      message("Copying script files (if any)...")
      errs <- errs + !all(copy_pkg_files(x = basename(out.dir),
        to = opt$target, ignore = opt$exclude))
    }
  }

}


quit(status = errs)


