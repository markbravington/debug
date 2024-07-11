\name{tcltk_window_shaker}
\alias{tcltk_window_shaker}
\title{Display fix for tcltk window in debug package}
\description{
In some versions of R{} on some platforms, there is a display problem with the tcltk window which should show your source code when your \code{\link{mtrace}}d source code is activated: the window comes up blank or incomplete or whatever. The issue has come and gone for me erratically over the years with new R{} versions; basically, the linke from R{} \code{tcltk} is a bit flaky. If it happens to you, try setting \code{options( debug.post.window.lauch.hook=debug::tcltk_window_shaker)}. The alternative is manual shrinking/maximizing/resizing/noodling around with every new tcltk window that the \pkg{debug} package opens... which works eventually, but is very tedious. \code{tcltk_window_shaker} tries to automate that.

If \code{tcltk_window_shaker} doesn't work, there are other tricks that might help; see \code{package?debug}, section "Display bugs", for (slightly) more info and hints on how to write your own.

There might also be a focus problem, whereby focus is left with the tcltk window rather than back in the R{} console/main input panel. On Windows only, \code{tcltk_window_shaker} will attempt to call \code{grDevices::bringToTop} iff package \pkg{grDevices} is already loaded (not necessarily attached). Hopefully that doesn't cause problems from Rstudio etc.
}
\usage{
# You shouldn't be calling this function yourself;
# ... use it only via options(), as per the DESCRIPTION.
tcltk_window_shaker(tcltk_window_name, tcl.win)
}
\arguments{
You probably don't need to know this, but any function that is passed as option \code{debug.post.window.lauch.hook} should expect two parameters. The first parameter will be a window title (character), which can potentially be used to find the window in the "desktop manager" and fiddle with it, as above. The second parameter is the tcltk window itself, which might be easier to fiddle with--- at your own risk.
\item{ tcltk_window_name,  tcl.win}{things you shouldn't tinker with.}
}
\examples{
\dontrun{
options( debug.post.window.launch.hook=debug::tcltk_window_shaker)
}
}
\keyword{misc}
