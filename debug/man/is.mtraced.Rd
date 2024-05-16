\name{is.mtraced}
\alias{is.mtraced}
\title{Check if a function has been 'mtrace'd.}
\description{Check if a function has been \code{\link{mtrace}}d.}
\usage{
is.mtraced(f)
}
\arguments{\item{ f}{a function}}
\value{TRUE or FALSE.}
\examples{
fff <- function() 99
is.mtraced( fff)
mtrace( fff)
is.mtraced( fff)
}
\seealso{\code{\link{mtrace}}}
\keyword{debugging}
