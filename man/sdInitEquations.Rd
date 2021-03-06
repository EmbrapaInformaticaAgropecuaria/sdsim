% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{sdInitEquations}
\alias{sdInitEquations}
\title{Initialize R-format Equations from a List of Strings or Expressions}
\usage{
sdInitEquations(equations, separator = "<-", eqName = c("aux", "eq"))
}
\arguments{
\item{equations}{A list of strings or R-expressions with the equations in 
R-format.}

\item{separator}{The assignment symbol used in the \code{equations} strings. 
Default is "<-".}

\item{eqName}{The name of the list that will be initialized and sorted, e.g.
'aux' for \code{\link{sdModelClass}} auxiliary equations and/or 'eq' for 
\code{\link{sdStaticModelClass}} algebraic equations. 

Can be a vector with all the equations list names that should be considered 
in the sorting. Default is c('aux', 'eq').}
}
\value{
A sorted list of equations in R-expression format.
}
\description{
Initialize a list of equations from a list of strings or R-expressions with 
the equations in R-format and sort the equations using their 
interdependencies to enable the reuse of evaluated equations in other 
equations further in the list. 
Abort if any circular dependency is found and warn the user.
Each equation will be an element of the returned sorted list in R-expression 
format. Use the \code{\link[base]{eval}} function to evaluate each 
R-expression in a specified environment.
}
\details{
The returned list of equations can be: added to the auxiliary equations list
of a \code{\link{sdModelClass}} object to replace the use of functions to 
calculate intermediary values of the model's system of differential 
equations; 
or added to the algebraic equations of a \code{\link{sdStaticModelClass}} 
object.
}
\examples{
const = list(c1 = 2, c2 = 1)
vars = list(k1 = 10, k2 = 2)

auxEquationsStrings <- list("cDiffFrac <- (const$c1 - const$c2) / aux$kDiff",
                            kDiff = "vars$k1 - vars$k2")
aux <- sdInitEquations(auxEquationsStrings, eqName = 'aux')

print(aux)
#> $kDiff
#> expression(vars$k1 - vars$k2)
#>
#> $cDiffFrac
#> expression((const$c1 - const$c2) / aux$kDiff)

aux$kDiff <- eval(aux$kDiff)
print(eval(aux$cDiff))
#> [1] 0.125
}
