#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# check_code.R -- Rscript script to check some aspects of the R coding style.
#   Part of the pkgutils package. See the manual for details.
#
# Package directory names can be provided at the command line. If none are
# given, the working directory is checked for subdirectories that contain a 
# DESCRIPTION file, and it is then attempted to check the contained R code
# files that.
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


for (lib in c("pkgutils", "optparse"))
  library(lib, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)


################################################################################


option.parser <- OptionParser(option_list = list(

  make_option(c("-b", "--blanks"), type = "integer", default = 2L,
    help = "Number of spaces per indention unit in R code [default: %default]",
    metavar = "NUMBER"),

  # A bug in Rscript causes '-g' to generate strange warning messages.
  # See https://stat.ethz.ch/pipermail/r-devel/2008-January/047944.html
  make_option(c("-g", "--good"), type = "character", default = "",
    help = 
      "R code files to not check, comma-separated list [default: %default]",
    metavar = "LIST"),

  # h is reserved for help!

  make_option(c("-j", "--jspaces"), type = "integer", default = 1L,
    help = 
      "Number of spaces starting Roxygen-style comments [default: %default]",
    metavar = "NUMBER"),

  make_option(c("-l", "--logfile"), type = "character",
    default = "check_code.log",
    help = "Logfile to use for problem messages [default: %default]",
    metavar = "FILE"),

  make_option(c("-m", "--modify"), action = "store_true", default = FALSE,
    help =
      "Potentially modify R sources when style checking [default: %default]"),

  make_option(c("-w", "--width"), type = "integer", default = 80L,
    help = "Maximum allowed line width in R code [default: %default]",
    metavar = "NUMBER")

))


opt <- parse_args(option.parser, positional_arguments = TRUE)
files <- opt$args
opt <- opt$options


if (opt$help || !length(files)) {
  print_help(option.parser)
  quit(status = 1L)
}


opt$good <- basename(unlist(strsplit(opt$good, ",", fixed = TRUE)))


################################################################################


pkgutils::logfile(opt$logfile)

m <- pkgutils::check_R_code(x = files, lwd = opt$width, indention = opt$blank,
  roxygen.space = opt$jspaces, modify = opt$modify, ignore = opt$good)

if (any(m))
  message(paste(sprintf("file '%s' has been modified", names(m)[m]),
    collapse = "\n"))





