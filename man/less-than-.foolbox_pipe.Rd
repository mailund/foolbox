% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform-dsl.R
\name{<.foolbox_pipe}
\alias{<.foolbox_pipe}
\title{This operator is used together with \code{\link{rewrites}} to transform a function
after it is defined and before it is assigned to a name.}
\usage{
\method{<}{foolbox_pipe}(pipe, fn)
}
\arguments{
\item{pipe}{A specificiation of a a pipeline of transformations provided
using the subscript operator to \code{\link[=rewrites]{rewrites()}}.}

\item{fn}{The function we wish to transform.}
}
\description{
This operator is used together with \code{\link{rewrites}} to transform a function
after it is defined and before it is assigned to a name.
}
\examples{
# This is a very simple inline function that require we
# provide the function body as it should be inserted.
# For a more detailed version, see the Tutorial vignette.
inline <- function(f, fn, body) {
   body <- substitute(body)
   rewrite(f) \%>\%
     rewrite_with(
         rewrite_callbacks() \%>\%
           add_call_callback(fn, function(expr, ...) body)
     )
}

g <- function(x) x**2
h <- rewrites[inline(g,y**2)] < function(y) y + g(y)
h

}
\seealso{
\code{[.foolbox_rewrite_spec}

rewrites
}
