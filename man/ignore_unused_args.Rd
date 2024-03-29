% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descr.R
\name{ignore_unused_args}
\alias{ignore_unused_args}
\title{do.call but without an error for unused arguments}
\usage{
ignore_unused_args(what, args)
}
\arguments{
\item{what}{either a function or a non-empty character string naming the function to be called.}

\item{args}{a list of arguments to the function call. The names attribute of args gives the argument names.}
}
\value{
The result of the (evaluated) function call.
}
\description{
do.call but without an error for unused arguments
}
\examples{
# works:
DescrTab2:::ignore_unused_args(
  chisq.test,
  list(x = factor(c(1, 0, 1, 1, 1, 0)), y = factor(c(0, 1, 1, 0, 1, 0)), abc = 3)
)

# would produce error:
# do.call(chisq.test, list(x=factor(c(1,0,1,1,1,0)), y=factor(c(0,1,1,0,1,0)), abc=3 ) )
}
