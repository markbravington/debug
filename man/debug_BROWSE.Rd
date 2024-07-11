\name{debug_BROWSE}
\alias{debug_BROWSE}
\title{Debugging support}
\description{
Suppose you are trying to debug a function, but you don't want to \code{\link{mtrace}} it (eg because the \code{\link{mtrace}}d version is too slow). If you know roughly where the problem manifests itself (eg via \code{traceback()} after an error), you can insert a call to \code{debug_BROWSE()}--- which is like \emph{provisionally} inserting \code{browser()}, but with the functionality of the \pkg{debug} package. The call will do nothing until you "awaken" the sleeper with \code{mtrace(debug_BROWSE)}. If triggered subsequently, it will pop up a debug window where you can inspect/change the variables in your main function; as usual in package \pkg{debug}, any expression you type is evaluated in the running environment of your main function. However, you can't actually step back into the main function, of course, because you haven't \code{\link{mtrace}}d it.

While I prefer \code{mtrace()} 99\% of the time, occasionally I'll use \code{debug_BROWSE()} if I have error-trap code that's getting triggered when I don't expect it. See \bold{Examples}.

Call \code{debug_BROWSE()} has no effect unless \code{mtrace(debug_BROWSE)} has been set, so you can safely leave it in. If your function is inside a package, you might need \code{debug::debug_BROWSE()}; remember to comment it out before CRANtorting your package.
}
\usage{
# Never ever call it like this... the next line exists only because of Rd rules
debug_BROWSE( nlocal = sys.parent())
# just do this...
# debug_BROWSE()
}
\arguments{\item{ nlocal}{See \code{\link{mlocal}} if you must know. I wouldn't bother, though.}
}
\examples{
\dontrun{
surprise <- function( weeble) \{
  result <- sqrt( weeble)
  if( !all( is.finite( result))) \{
    browse_DEBUG() # now you can look at weeble
stop( "WTF?")
  \}
return( result)
\}
mtrace( debug_BROWSE)
surprise( 1:10 - 3)
}
}
\keyword{misc}
