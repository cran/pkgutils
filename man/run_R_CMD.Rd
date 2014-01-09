\name{run_R_CMD}
\alias{copy_pkg_files}
\alias{copy_pkg_files.character}
\alias{delete_o_files}
\alias{delete_o_files.character}
\alias{run_R_CMD}
\alias{run_R_CMD.character}
\title{Run \command{R CMD}}
\usage{
  run_R_CMD(x, ...)

  \method{run_R_CMD}{character} (x, what = "check", ...,
    sudo = identical(what, "INSTALL"),
    system.args = list())

  copy_pkg_files(x, ...)

  \method{copy_pkg_files}{character} (x, what = "scripts",
    to = file.path(Sys.getenv("HOME"), "bin"),
    installed = TRUE, ignore = NULL, overwrite = TRUE, ...)

  delete_o_files(x, ...)

  \method{delete_o_files}{character} (x, ext = "o",
    ignore = NULL, ...)
}
\arguments{
  \item{x}{For \code{run_R_CMD}, a character vector with
  arguments passed to \command{R CMD <what>}. If these are
  the names of one to several package directories, the
  package name alone does \strong{not} suffice unless the
  package is a subdirectory of the working directory.

  For \code{copy_pkg_files} and \code{delete_o_files}, a
  character vector passed as eponymous argument to
  \code{\link{pkg_files}}.}

  \item{what}{Character scalar with the name of the command
  used by \command{R CMD}, or (for \code{copy_pkg_files}) a
  character vector with the names of subdirectories to
  copy.}

  \item{sudo}{Logical scalar. Prepend a \command{sudo} call
  to the command?  Probably makes only sense on UNIX-like
  systems.}

  \item{system.args}{Optional list of arguments passed to
  \code{system} from the \pkg{base} package.}

  \item{to}{Character vector indicating the target
  folder(s) or file(s).}

  \item{overwrite}{Logical scalar passed to
  \code{file.copy} from the \pkg{base} package (in addition
  to \code{from} and \code{to}).}

  \item{ext}{Character vector with file extensions to
  consider. Need not contain the leading dot. Content of
  \code{.Platform$dynlib.ext} is added automatically.}

  \item{...}{For \code{run_R_CMD}, optional command-line
  switches passed to \command{R CMD <what>}. In contrast to
  \code{x}, leading dashes are automatically prepended as
  necessary. For \code{copy_pkg_files}, optional arguments
  passed to \code{file.copy} from the \pkg{base} package.}

  \item{installed}{Logical scalar. If \code{TRUE}, the
  package(s) are searched using \code{find.package}.
  Otherwise \code{pkg} is treated as list of directories
  and/or file names, distinguished using
  \code{is_pkg_dir}.}

  \item{ignore}{\code{NULL} or a character vector of file
  names (without their directory-name parts) to remove from
  the result, or a list. Ignored if empty. If a non-empty
  character vector, matching is done case- insensitively.
  If a list, used as arguments passed to \code{grep}
  (except for \code{x}, \code{value} and \code{invert}).
  Enclosing \code{ignore} in \code{I()} reverts the
  action.}
}
\value{
  The return value of \code{run_R_CMD} is the one of the
  call of \command{R CMD}, depending on \code{system.args},
  by default an integer indicating success or failure.

  \code{copy_pkg_files} returns a logical vector. See
  \code{file.copy} from the \pkg{base} package for details.

  \code{delete_o_files} also returns a logical vector, this
  time indicating whether deletion succeeded. See
  \code{file.remove} from the \pkg{base} package.
}
\description{
  Externally call \command{R CMD}, e.g. for checking or
  installing \R packages, or conduct some postprocessing
  after checking or installation.
}
\details{
  Windows users might need to install \pkg{Rtools} for
  \code{run_R_CMD} to work, see
  \url{http://cran.r-project.org/bin/windows/Rtools/}.

  \code{copy_pkg_files} copies package files after
  installation. This is mainly intended for script files,
  which should often be placed in a directory for
  executable files. The \command{docu.R} and
  \command{merge.R} scripts that come with the
  \pkg{pkgutils} package are examples for such files.

  \code{delete_o_files} removes object files in the
  \file{src} subdirectory of a package remaining from
  previous compilation attempts, if any.
}
\examples{
# Running R CMD <what>: see the 'docu.R' script provided with this package,
# options '--check', # '--install' and '--yes'.

# Copying files: see the 'docu.R' script provided with this package, options
# '--target' and '--exclude'.

# Deleting object files: see the 'docu.R' script provided with this package,
# option '--zapoff'.
}
\seealso{
  base::system base::file.copy base::file.remove

  Other package-functions: \code{\link{check_R_code}},
    \code{\link{check_Sweave_start}},
    \code{\link{is_pkg_dir}},
    \code{\link{logfile}},   \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{swap_code}},
  }
\keyword{package}

