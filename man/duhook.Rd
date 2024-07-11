\name{duhook}
\alias{duhook}
\title{Call user hook for debugging window}
\description{Package \pkg{debug} lets you set a "user hook" to be called when a new TCL/TK debugging window gets opened. This can be useful e.g. to do some manual wiggling to get TCL/TK to synch properly. Sometimes you might want to call your own user hook deliberately, e.g. if the hook hasn't managed to synch automatically. \code{duhook} lets you do that. See DISPLAY stuff in \code{package?debug}.
}
\usage{duhook(D = 0)
}
\arguments{\item{ D}{Frame number (displayed in the window's title) to call the hook on. By default, it's the most-recently displayed window.}
}
\seealso{\code{package?debug}, \code{\link{tcltk_window_shaker}}
}
\keyword{misc}
