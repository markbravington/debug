\name{debug.C}
\alias{debug.C}
\title{For debugging C-calls with many arguments.}
\description{
Sometimes you call \code{.C} with huge numbers of arguments (e.g. when initializing data structures), and you get an error message complaining about one of the arguments. Trouble is, the error message doesn't tell you which one, and it can be hard to track down. A convenient way to find out is to \code{\link{mtrace}} the caller and run as far as the \code{.C} call, then do this at the prompt:

\preformatted{
  D(n)> .C <- debug.C
}

and then hit <ENTER>. All \emph{named} arguments will be evaluated and summary information will be printed. So you do need to make sure your "important" \code{.C} arguments all have names-- which is good practice anyway, for matching up the R{} and C code. Return value is \code{NULL}; \code{debug.C} doesn't actually try to run any C code. (You wouldn't be using this if your \code{.C} worked!)
}
\usage{
# This is mandatory, but not useful here. See *Description*
# You would never call 'debug.C' directly
debug.C(...)
}
\arguments{\item{ ...}{a la \code{.C}}
}
\keyword{misc}
