\name{debug_knitr}
\alias{debug_knitr}
\title{Debugging knitr documents}
\description{
\bold{NOT WORKING} in R4.4, and should be disabled by default in package \pkg{debug} >= v1.4. To be fixed...

To debug a \code{knitr} document (during compilation, i.e. while it's on its way to become PDF or HTML), put one line into the first chunk:

\preformatted{
  debug::debug_knitr( mtrace=TRUE)
}

Then, every time it gets to a code chunk, a "consolette" should pop up to allow you to interact with the evaluation, just as if you had \code{\link{mtrace}}d a function (or, more accurately, as if you were \code{\link{mrun}}ning the code in the chunk with argument \code{debug=TRUE}).

What that line actually does, is automatically load the package \emph{and} set some options needed for it to work easily. It also turns on \code{\link{mtrace}} for all chunks (because of the \code{TRUE}), but you can override that per-chunk with eg \code{mtrace=FALSE} in the chunk options. \code{mtrace=NA} is also useful; see below.

Default use of \code{debug_knitr} does change a few \code{knitr} options and hooks, explained below. If you don't want that, you can set some parameters as explained below, but then you're in charge of making the debugging actually happen. You might also need to set \code{cache=FALSE} to make the consolette appear.

When everything is working (or at least running), I just disable that line by putting \code{#} at the start. Setting \code{mtrace=FALSE} will stop the consolette appearing, but you may find that the output isn't right (e.g. currently it's only showing results not commands--- it's on the To-Do list, but it's got a lot of company).

\emph{Before} the above line, you can also set some \code{options} to control the placement of the consolette (the window that lets you interact with the debugging process). Obvious ones are \code{debug.height} and \code{debug.screen.pos}. They are safe to leave, even if you disable the \code{debug_knitr} line. There is a little more info about the consolette in \code{package?debug}.
}
\usage{debug_knitr(auto = TRUE, readLines_hack = TRUE, mtrace = TRUE)
}
\arguments{
\item{ auto}{?globally modify the \code{code} chunk-option so that all "normal" chunks will be \code{mtrace}d, or not according to the level of \code{opts_chunk$get("mtrace")}.}
\item{ readLines_hack}{?globally modify \code{readLines} so that it will auto-debug a chunk read from external file, with \code{code=readLines("<myfile>")}? Operation should be unchanged otherwise.}
\item{ mtrace}{default level of \code{mtrace} option. \code{TRUE}, \code{FALSE}, or \code{NA} which is like \code{TRUE} except that the debugger won't stop unless there's an error (because the default breakpoint on line 1 gets turned off).}
}
\seealso{\code{package?debug}, non-existent vignette
}
\examples{
\dontrun{
# Put this in the first chunk of your knitr doco for an au
debug::debug_knitr()
# If you want "only-on-crash" debugging:
debug::debug_knitr( mtrace=NA)
# or chunk-by-chunk, EG :
# <<mychunk, mtrace=FALSE, ...>>= # run this chunk normally, ie without mtrace
}
}
\keyword{misc}
