% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static-analysis.R
\name{collect_assigned_symbols_in_expression}
\alias{collect_assigned_symbols_in_expression}
\alias{collect_assigned_symbols_in_function}
\title{Extracts all the symbols that appear on the left-hand side of an
assignment.}
\usage{
collect_assigned_symbols_in_expression(expr, env, params = list(),
  topdown = list())

collect_assigned_symbols_in_function(fun, topdown = list())
}
\arguments{
\item{expr}{The expression to analyse}

\item{env}{Environment in which to look up symbols.}

\item{params}{Parameters for the function being analysed (if
these are needed).}

\item{topdown}{Information to pass top-down in the traversal.}

\item{fun}{The function whose body we should analyse}
}
\value{
A list containing the symbols that were assigned to.
}
\description{
Since R does not require that we declare local variables, and since
the variables that are assigned to a local scope depend on the runtime
execution of functions, we cannot determine with any certainty which
variables will be assigned to in any given scope at any given program
point. So the best we can do is figure out which variables are
\emph{potentially} assigned to. Which is what this function does.
}
\details{
The \code{\link[=collect_assigned_symbols_in_function]{collect_assigned_symbols_in_function()}} function reformats the collected
data into a character vector, removes duplications, and remove the
formal parameters of the function from the list, so those are not considered
local variables (rather, they are considered formals and presumably handled
elsewhere as such).
}
\section{Functions}{
\itemize{
\item \code{collect_assigned_symbols_in_expression}: Analyse an expression.

\item \code{collect_assigned_symbols_in_function}: Analyse the body of a function.
}}

