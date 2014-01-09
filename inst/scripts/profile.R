#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# profile.R -- Rscript script for non-interactively profiling R code.
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


for (lib in c("tools", "optparse"))
  library(lib, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)


make_outfiles <- function(x, opt) {
  if (opt$stdout)
    return(rep.int("/dev/stdout", length(x)))
  x <- sprintf("%s.%s", file_path_sans_ext(x, TRUE), opt$extension)
  if (nzchar(opt$directory))
    file.path(opt$directory, basename(x))
  else
    x
}


parse_arg_listing <- function(x) {
  unique.default(unlist(strsplit(x, ",", TRUE), FALSE, FALSE))
}


################################################################################


option.parser <- OptionParser(option_list = list(

  make_option(c("-d", "--directory"), type = "character", default = ".",
    help = "Output directory (empty => input directory) [default: %default]",
    metavar = "STR"),

  make_option(c("-e", "--extension"), type = "character", default = "out",
    help = "Output file extension [default: %default]",
    metavar = "STR"),

  make_option(c("-l", "--libraries"), type = "character", default = "",
    help = "Comma-separated list of R libraries to load [default: %default]",
    metavar = "STR"),

  make_option(c("-r", "--replicates"), type = "integer", default = 100L,
    help = "Number of replicates when profiling [default: %default]",
    metavar = "NUM"),

  make_option(c("-s", "--stdout"), action = "store_true", default = FALSE,
    help = "Send output to STDOUT [default: %default]")

))


################################################################################


opt <- parse_args(option.parser, positional_arguments = TRUE)
infiles <- opt$args
opt <- opt$options


if (opt$help || !length(infiles)) {
  print_help(option.parser)
  quit(status = 1L)
}


opt$libraries <- parse_arg_listing(opt$libraries)
for (lib in opt$libraries)
  library(lib, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)


################################################################################


# Note that other output modes are possible. For instance, sourceing all input
# files, then calling a specified function.

outfiles <- make_outfiles(infiles, opt)

for (i in seq_along(infiles)) {
  expr <- parse(infiles[i])
  Rprof(outfiles[i])
  for (i in seq_len(opt$replicates))
    eval(expr)
  Rprof(NULL)
}


################################################################################


