\name{get.retval}
\alias{get.retval}
\title{Show current return value when debugging}
\description{When debugging a function, \code{get.retval} gives the current return value, i.e. the result of the most recent valid statement executed in the function body, whether typed or in the original code, and excluding calls to \code{\link{go}} etc.. Presumably only relevant when in exit code.}
\usage{
get.retval()
}
\details{To \bold{change} the return value while in exit code, use \code{\link{skip}} to move back into the function code, then \code{return(whatever)}.}
\seealso{\code{\link{skip}}, \code{\link{go}}, \code{\link{last.try.error}}}
\author{Mark Bravington}
\keyword{debugging}
