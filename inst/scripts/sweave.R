#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# sweave.R -- Rscript script to non-interactively run Sweave. Part of the 
#   pkgutils package.
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################



# In conjunction with Rscript, Sweave does not understand S4 methods unless 
# 'methods' is explictly loaded.
#
library(methods, quietly = TRUE, warn.conflicts = FALSE)


files <- commandArgs(trailingOnly = TRUE)

if (!length(files)) {
  cat("Usage: sweave.R Rnw-file(s)\n")
  quit(status = 1L)
}


invisible(lapply(files, utils::Sweave))

