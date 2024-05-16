\name{last.try.error}
\alias{last.try.error}
\title{Get last try-error}
\description{Suppose you are debugging, and have stepped-into a \code{try} statement, and the debugger has stopped at an error. You might actually want to return that error as an object of class \code{"try-error"}, just like \code{try} would if you weren't debugging. To do that, type \code{return( last.try.error())} at the debug prompt.}
\usage{
# Normally you would type return( last.try.error()) at the debug prompt
last.try.error()
}
\value{An object of class \code{try-error}, just like \code{try} itself returns in case of error.}
\keyword{misc}
