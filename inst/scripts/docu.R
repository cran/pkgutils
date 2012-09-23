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


option.parser <- OptionParser(option_list = list(

  make_option(c("-a", "--ancient"), action = "store_true", default = FALSE,
    help = "Do not update the DESCRIPTION file [default: %default]"),

  make_option(c("-b", "--blanks"), type = "integer", default = 2L,
    help = "Number of spaces per indention unit in R code [default: %default]",
    metavar = "NUMBER"),

  make_option(c("-c", "--check"), action = "store_true", default = FALSE,
    help = "Run 'R CMD check' after documenting [default: %default]"),

  make_option(c("-d", "--delete"), type = "character", default = "",
    help = "Subdirectories to delete, colon-separated list [default: %default]",
    metavar = "LIST"),

  make_option(c("-e", "--exec"), type = "character", default = "ruby",
    help = "Ruby executable used if -p or -s is chosen [default: %default]",
    metavar = "FILE"),

  make_option(c("-f", "--format"), type = "character", default = "%Y-%m-%d",
    help = "Format of the date in the DESCRIPTION file [default: %default]",
    metavar = "STR"),

  # A bug in Rscript causes '-g' to generate strange warning messages.
  # See https://stat.ethz.ch/pipermail/r-devel/2008-January/047944.html
  make_option(c("-g", "--good"), type = "character", default = "",
    help = 
      "R code files to not check, comma-separated list [default: %default]",
    metavar = "LIST"),

  # h is reserved for help!

  make_option(c("-i", "--install"), action = "store_true", default = FALSE,
    help = "Also run 'R CMD INSTALL' after documenting [default: %default]"),

  make_option(c("-j", "--jspaces"), type = "integer", default = 1L,
    help = 
      "Number of spaces starting Roxygen-style comments [default: %default]",
    metavar = "NUMBER"),

  make_option(c("-k", "--keep"), action = "store_true", default = FALSE,
    help = "Keep the version number in DESCRIPTION files [default: %default]"),

  make_option(c("-l", "--logfile"), type = "character", default = "docu_%s.log",
    help = "Logfile to use for problem messages [default: %default]",
    metavar = "FILE"),

  make_option(c("-m", "--modify"), action = "store_true", default = FALSE,
    help =
      "Potentially modify R sources when style checking [default: %default]"),

  make_option(c("-n", "--nosudo"), action = "store_true", default = FALSE,
    help = "In conjunction with -i, do not use sudo [default: %default]"),
  
  make_option(c("-o", "--options"), type = "character", default = "as-cran",
    help = "'R CMD check' options, comma-separated list [default: %default]",
    metavar = "LIST"),

  make_option(c("-p", "--preprocess"), action = "store_true", default = FALSE,
    help = "Preprocess R files using code swapping [default: %default]"),

  make_option(c("-q", "--quick"), action = "store_true", default = FALSE,
    help = "Do not remove duplicate 'seealso' links [default: %default]"),

  make_option(c("-r", "--remove"), action = "store_true", default = FALSE,
    help = "First remove output directories if distinct [default: %default]"),

  make_option(c("-s", "--s4methods"), action = "store_true", default = FALSE,
    help = "Repair S4 method descriptions [default: %default]"),

  make_option(c("-t", "--target"), type = "character",
    default = file.path(Sys.getenv("HOME"), "bin"),
    help = paste("For -i, script file target directory; ignored if",
      "empty [default: %default]"), metavar = "DIR"),

  make_option(c("-u", "--unsafe"), action = "store_true", default = FALSE,
    help = "In conjunction with -i, omit checking [default: %default]"),

  make_option(c("-v", "--verbatim"), action = "store_true", default = FALSE,
    help = paste("No special processing of package names ending in '*_in'",
      "[default: %default]")),

  make_option(c("-w", "--width"), type = "integer", default = 80L,
    help = "Maximum allowed line width in R code [default: %default]",
    metavar = "NUMBER"),

  make_option(c("-x", "--exclude"), type = "character", default = "",
    help = paste("Files to ignore when using -t, comma-separated list",
      "[default: %default]"), metavar = "LIST"),

  make_option(c("-y", "--yes"), action = "store_true", default = FALSE,
    help = "Yes, also build the package [default: %default]"),

  make_option(c("-z", "--zapoff"), action = "store_true", default = FALSE,
    help = "Do not zap object files in 'src', if any [default: %default]")

))


opt <- parse_args(option.parser, positional_arguments = TRUE)
package.dirs <- opt$args
opt <- opt$options


if (opt$help) {
  print_help(option.parser)
  quit(status = 1L)
}


opt$options <- unlist(strsplit(opt$options, ",", fixed = TRUE))
opt$delete <- unlist(strsplit(opt$delete, ":", fixed = TRUE))
opt$good <- basename(unlist(strsplit(opt$good, ",", fixed = TRUE)))
opt$exclude <- unlist(strsplit(opt$exclude, ",", fixed = TRUE))


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

  message("Checking R code of", msg)
  tmp <- pkgutils::check_R_code(out.dir, lwd = opt$width,
    indention = opt$blank, roxygen.space = opt$jspaces, modify = opt$modify,
    ignore = opt$good)
  errs <- errs + length(which(is.na(tmp)))
  if (any(tmp))
    message(paste("Modifying file", names(tmp)[tmp], collapse = "\n"))

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


