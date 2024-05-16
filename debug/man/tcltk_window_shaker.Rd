\name{tcltk_window_shaker}
\alias{tcltk_window_shaker}
\title{Display fix for tcltk window in debug package}
\description{
In some versions of R{} on some platforms, there is a display problem with the tcltk window that should show your source code when your \code{\link{mtrace}}d source code is activated: the window comes up blank or incomplete or whatever. If this happens to you, try setting \code{options( debug.post.window.lauch.hook=debug::tcltk_window_shaker)}. The alternative is manual shrinking/resizing/noodling around with every new tcltk window that the \pkg{debug} package opens...

There might also be a focus problem, whereby focus is left with the tcltk window rather than back in the R{} console/main input panel. On Windows only, \code{tcltk_window_shaker} will attempt to call \code{grDevices::bringToTop} iff package \pkg{grDevices} is already loaded (not necessarily attached). Hopefully that doesn't cause problems from Rstudio etc.

This is currently (i) necessary and (ii) working for me with R3.6.2 Rgui (32 and 64 bit versions).

See \code{package?debug}, section "Display bugs", for (slightly) more info and hints on how to write your own, in case this one doesn't work.
}
\usage{
# You shouldn't be calling this function yourself;
# ... use it only via options(), as per the DESCRIPTION.
tcltk_window_shaker(tcltk_window_name, tcl.win)
}
\arguments{
You don't need to know this, but any function that is passed as option \code{debug.post.window.lauch.hook} should expect two parameters. The first parameter will be a window title (character), which can potentially be used to find the window in the "desktop manager" and fiddle with it, as above. The second parameter is the tcltk window itself, which might be easier to fiddle with--- at your own risk.
\item{ tcltk_window_name,  tcl.win}{things you shouldn't tinker with.}
}
\examples{
\dontrun{
options( debug.post.window.launch.hook=debug::tcltk_window_shaker)
}
}
\keyword{misc}
