\name{must}
\alias{must}
\title{Convert warnings to errors}
\usage{
  must(expr, msg = NULL, ..., domain = NULL)
}
\arguments{
  \item{expr}{\R expression to evaluate.}

  \item{msg}{Character vector to be used as error message.
  If empty or \code{NULL}, the \code{conditionMessage} of
  the issued warning is used.}

  \item{...}{Optional further arguments to
  \code{tryCatch}.}

  \item{domain}{Passed to \code{stop} (if a warning
  occurs).}
}
\value{
  The result of \code{expr} (if no error occurs).
}
\description{
  Raise an error if a warning occurs. Useful for making
  certain tests more strict. It is a bit easier to use than
  changing the \sQuote{warn} entry of \code{options} from
  the \pkg{base} package (because the entry would usually
  need to be set back).
}
\examples{
(x <- try(must(as.numeric(c("1", "2", "3"))), silent = TRUE))
stopifnot(identical(x, c(1, 2, 3)))
(x <- try(must(as.numeric(c("1", "x", "3"))), silent = TRUE))
stopifnot(inherits(x, "try-error"))
}
\seealso{
  base::tryCatch base::stop base::options

  Other auxiliary-functions: \code{\link{L}},
  \code{\link{LL}}, \code{\link{case}},
    \code{\link{flatten}},   \code{\link{listing}},       \code{\link{logfile}},   \code{\link{map_files}},
    \code{\link{run_ruby}},     \code{\link{sections}},
    }
\keyword{utilities}

