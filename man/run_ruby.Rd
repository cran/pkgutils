\name{run_ruby}
\alias{run_ruby}
\alias{run_ruby.NULL}
\alias{run_ruby.character}
\alias{run_ruby.numeric}
\title{Run a Ruby script}
\usage{
  run_ruby(x, ...)

  \method{run_ruby}{NULL} (x, ruby = "ruby", ...)

  \method{run_ruby}{numeric} (x, args = "-w",
    ruby = "ruby", ...)

  \method{run_ruby}{character} (x, args = "-w",
    ruby = "ruby", ...)
}
\arguments{
  \item{x}{Character vector containing the name of a script
  and optionally, after that name, the script's arguments.
  If a numeric vector, a required minimum Ruby version. A
  command is then constructed that only results if this
  version requirement is met. If \code{NULL}, the path to
  the Ruby executable is returned, or an empty string if
  this is not found.}

  \item{args}{Character vector with arguments passed to
  Ruby before the content of \code{x}. \sQuote{--} is
  appended automatically. Note that any \sQuote{-e}
  argument would cause a character vector \code{x} to be
  ignored, and that otherwise an empty \code{x} character
  vector would cause the Ruby process to hang (wait for
  input that will not arrive).}

  \item{ruby}{Character scalar containing the name of the
  Ruby executable. It is an error if this file is not found
  using \code{Sys.which}.}

  \item{...}{Optional arguments (except \sQuote{command})
  passed to \code{system} from the \pkg{base} package.}
}
\value{
  Unless \code{x} is \code{NULL}, the result of a call to
  \code{system}. This is an integer scalar unless
  \code{\dots} dictates otherwise.
}
\description{
  Run Ruby with an externally provided Ruby script or with
  code provided at the command line with \sQuote{-e}.
}
\examples{
if (nzchar(run_ruby(NULL))) {
  # run a dummy Ruby command that does nothing
  (x <- run_ruby(x = character(), args = "-e'nil'"))
  stopifnot(identical(x, 0L))
} else {
  warning("cannot find 'ruby'")
}
}
\seealso{
  base::system base::Sys.which

  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{flatten}},   \code{\link{listing}},
          \code{\link{logfile}},   \code{\link{map_files}},
  \code{\link{must}}
}
\keyword{interface}

