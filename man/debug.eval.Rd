\name{debug.eval}
\alias{debug.eval}
\title{Evaluate expression in debugging window}
\description{
You shouldn't need to call this directly--- calls to \code{eval()} while debugging (either in code, or from the debug console) should divert automatically to this function. This documentation pre-dates the automated version, and you probably don't need to read it. But FWIW, the old documentation continues from here...

Like \code{eval} but summons up a debug window, as when debugging a function call, so that you can step thru \code{expr}. For use at the command-line while debugging where, if you just typed \code{eval(expr)} it would not step into \code{expr}. Step-in only happens if \code{expr} has multiple elements.

\preformatted{
  debug.eval( quote({ a <- 1})) #  won't step
  debug.eval( quote({ a <- 1; b <- 2})) #  will step
}

If you want to step thru a sequence of text commands (e.g. a script), see \code{\link{msource}} and \code{\link{mrun}}.

As explained under "SPECIAL FUNCTIONS" in \code{package?debug}, there are analogous "steppy" functions for \code{evalq} (i.e. \code{debug.evalq}), \code{with}, \code{within}, \code{local}, \code{suppressWarnings}, and \code{try}. However, these are currently not exported, so you'd need e.g. \code{debug:::debug.with( dataset, commands)}. I might export them eventually, but I suspect they're less likely to be used during "live" debugging.
}
\usage{
debug.eval( expr, envir = parent.frame(),
    enclos = if (is.list(envir) ||  is.pairlist(envir)) parent.frame() else baseenv())
}
\arguments{\item{ expr, envir, enclos}{see \code{\link{eval}}}
}
\seealso{\code{fixr}, \code{package?debug} under SPECIAL FUNCTIONS, \code{\link{msource}}, \code{\link{mrun}}
}
\keyword{misc}
