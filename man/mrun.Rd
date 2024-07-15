\name{mrun}
\alias{mrun}
\alias{mdrun}
\alias{mpdrun}
\alias{mdrunex}
\alias{msource}
\title{Run script with optional debugging}
\description{
Run an "R script" either from a file (\code{msource}) or directly from a character vector (\code{mrun}, \code{mdrun}, \code{mpdrun}) or from a package example (\code{mdrunex}). The main point is to allow debugging with \code{\link{mtrace}}, controlled via the \code{debug} argument. This defaults to TRUE for \code{msource} (where you could just use \code{source} instead if you didn't want to debug) and \code{mdrun} and \code{mpdrun} and \code{mdrunex}, but to FALSE for \code{mrun} (which is useful in its own right without debugging, in which case it amounts just to \code{eval( parse( text=...))}). Evaluation by default takes place in \code{.GlobalEnv}, so objects created are permanent.

I use \code{mrun} because, although scripts (as opposed to "pure functions") can be quite useful, I don't like separate script files--- too many Things to keep track of. Also, debugging "scripts" is normally painful compared to debugging functions, for which \code{\link{mtrace}} makes life easy. My "scripts" are created with \code{fixtext} as opposed to \code{fixr}, and are character vectors of S3 class \code{cat}, which means they display nicely; I give them names ending ".r", so that they are obvious. \code{mdrun} just saves me having to type \code{mrun( <greatbiglongname>, debug=TRUE)} which I was finding myself doing a lot of. \code{mpdrun} is convenient when a scriptlet doesn't parse completely (which might be deliberate); it lets you debug the first parsable portion, and saves me from typing \code{mdrun(<greatbiglongname>,partial_parse_OK=TRUE)}.

For package examples, you can use \code{utils::example} (qv), but I often want to be able to pause inside that code, etc. So \code{mdrunex} first uses \code{utils::example} to extract the example code, then \code{mdrun} to actually run it in the debugger (but still putting results into \code{.GlobalEnv}, FBOW). The arguments \code{run.dontrun} and \code{run.donttest} might be useful.

The debugging trick behind all this is to make the "script" into the body of an \code{mlocal} function--- which is thereby debuggable--- then forcing it to execute directly in the \code{local} environment. The "frame number" when debugging--- ie "xxx" inside the "D(xxx)>" prompt--- will be 0 for the default \code{local=.GlobalEnv}, or some unpredictable number for a bespoke environment.
}
\usage{
mrun( script, local=FALSE, debug=FALSE, echo=TRUE, print.eval=FALSE, partial_parse_OK=FALSE, ...)
mdrun( script, ...) # passed to mrun() with debug=TRUE
mdrunex( topic, package, ...)
msource( file, local=FALSE, debug=TRUE, ...)
}
\arguments{
\item{ script}{Character vector of R{} commands}
\item{ local}{FALSE for \code{.GlobalEnv}, TRUE for frame where \code{msource} / \code{mrun} is called, or an environment (or something that can be coerced to an environment)}
\item{ debug}{set to TRUE if you want to use the \pkg{debug} package}
\item{ topic, package}{strings (ie must be quoted)}
\item{ file}{filename of R{} "script"}
\item{ echo, print.eval, ...}{as per \code{source}, to which they are passed iff \code{debug=FALSE}. For \code{mdrun}, elements of \code{...} that match named args to \code{mrun} will be handled by \code{mrun} (so the sentence before this one is false in strict legalistic terms, but strict legalistic people don't deserve any better).}
\item{ partial_parse_OK}{if TRUE, don't just fail if the scriptlet fails to parse completely; rather, just start debugging on the bits at the front that do parse.}
}
\value{Whatever the last expression gave, invisibly.
}
\examples{
\dontrun{
# To use 'mdrunex', add argument 'run.dontrun=TRUE'
scriptio.r <- c( 'a <- 99', 'b <- a+1')
mrun( scriptio.r)
a # 99; created in .GlobalEnv
b # 100
leftovers <- 555555
mdrunex( 'mdrunex', 'debug') # NULL code, coz it's in a don't-run block
}
}
\keyword{misc}
