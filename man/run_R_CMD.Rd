\name{run_R_CMD}
\alias{run_R_CMD}
\alias{run_R_CMD.character}
\title{Run R CMD}
\usage{
  run_R_CMD(x, ...)

  \method{run_R_CMD}{character} (x, what = "check", ...,
    sudo = identical(what, "INSTALL"),
    system.args = list())
}
\arguments{
  \item{x}{Character vector with arguments passed to
  \sQuote{R CMD <what>}. If these are the names of one to
  several package directories, the package name alone does
  \strong{not} suffice unless the package is a subdirectory
  of the working directory.}

  \item{what}{Character scalar. The name of the subcommand
  of \sQuote{R CMD}.}

  \item{...}{Optional command-line switches passed to
  \sQuote{R CMD <what>}.  In contrast to \code{x}, leading
  dashes are automatically prepended as necessary.}

  \item{sudo}{Logical scalar. Prepend \sQuote{sudo} to the
  command? Probably makes only sense on UNIX-like systems.}

  \item{system.args}{Optional list of arguments passed to
  \code{system} from the \pkg{base} package.}
}
\value{
  The return value of the call of \sQuote{R CMD}, depending
  on \code{system.args}, by default an integer indicating
  success or failure.
}
\description{
  Externally call \sQuote{R CMD}, e.g. for checking \R
  packages.
}
\details{
  Windows users might need to install \sQuote{Rtools} for
  this to work, see
  \url{http://cran.r-project.org/bin/windows/Rtools/}.
}
\examples{
# See the 'docu.R' script provided with this package, options '--check',
# '--install' and '--yes'.
}
\seealso{
  base::system

  Other package-functions: \code{\link{check_R_code}},
    \code{\link{copy_pkg_files}},
    \code{\link{delete_o_files}},
    \code{\link{is_pkg_dir}},
    \code{\link{pack_desc}},
    \code{\link{pkg_files}},
    \code{\link{repair_S4_docu}},
    \code{\link{swap_code}},
  }
\keyword{package}

