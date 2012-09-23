#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# merge.R -- R script for merging CSV files. Part of the pkgutils package.
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


read_and_create_unique_names <- function(files, options) {
  data <- lapply(X = files, FUN = read.delim, sep = options$separator, 
    check.names = options$names, strip.white = !options$keep)
  suffixes <- tools::file_path_sans_ext(basename(files), compression = TRUE)
  cn <- lapply(data, colnames)
  ok <- rep(list(options$ycolumn), length(data))
  ok[[1L]] <- options$xcolumn
  for (i in seq_along(cn)) {
    twice <- cn[[i]] %in% setdiff(unlist(cn[-i]), ok[[i]])
    colnames(data[[i]])[twice] <- sprintf("%s.%s", cn[[i]][twice], suffixes[i])
  }
  data
}


################################################################################
#
# Option processing
#


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(c("-a", "--all"), action = "store_true",
    help = "Keep non-matching lines of file 2, too [default: %default]", 
    default = FALSE),

  optparse::make_option(c("-o", "--onename"), action = "store_true",
    help = 
      "Do not split arguments of '-x' and '-y' at ',' [default: %default]", 
    default = FALSE),

  optparse::make_option(c("-k", "--keep"), action = "store_true",
    help = "Keep whitespace surrounding the separators [default: %default]", 
    default = FALSE),

  optparse::make_option(c("-n", "--names"), action = "store_true",
    help = "Convert column names to syntactical names [default: %default]",
    default = FALSE),

  optparse::make_option(c("-s", "--separator"), type = "character",
    help = "Field separator in CSV files [default: '%default']",
    metavar = "SEP", default = "\t"),

  optparse::make_option(c("-u", "--unquoted"), action = "store_true",
    help = "Do not quote fields in output [default: %default]",
    default = FALSE),

  optparse::make_option(c("-x", "--xcolumn"), type = "character",
    help = "Name of the merge column in file 1 [default: '%default']",
    default = "Object", metavar = "COLUMN"),

  optparse::make_option(c("-y", "--ycolumn"), type = "character", 
    help = "Name of the merge column in file 2 [default: like file 1]",
    default = "", metavar = "COLUMN")

), usage = "%prog [options] csv_file_1 csv_file_2 ...", prog = "merge.R")


opt <- optparse::parse_args(option.parser, positional_arguments = TRUE)
files <- opt$args
opt <- opt$options
if (!opt$onename)
  opt$xcolumn <- unlist(strsplit(opt$xcolumn, ",", fixed = TRUE))
if (!nzchar(opt$ycolumn)) {
  opt$ycolumn <- opt$xcolumn
} else if (!opt$onename) {
  opt$ycolumn <- unlist(strsplit(opt$ycolumn, ",", fixed = TRUE))
}

################################################################################


if (length(files) < 2L) {
  message("\nmerge.R -- Merge CSV files based on specified columns.\n")
  optparse::print_help(option.parser)
  message("Note that non-matching lines of file 1 are always kept.\n")
  quit(status = 1L)
}


data <- read_and_create_unique_names(files, opt)

x <- data[[1]]
for (i in seq_along(data)[-1L])
  x <- merge(x, data[[i]], by.x = opt$xcolumn, by.y = opt$ycolumn, 
    all.x = TRUE, all.y = opt$all)

write.table(x, sep = opt$separator, row.names = FALSE, quote = !opt$unquoted)


################################################################################


