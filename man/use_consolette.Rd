\name{use_consolette}
\alias{use_consolette}
\title{Consolette}
\description{
You should haaaardly ever need to call these; the consolette is meant to be used iff R{} is running non-interactively, and should always be present (albeit not necessarily visible) if so. However, you \emph{might} prefer the consolette even during interactive use, so "manual" control might be useful (at least for me when I am debugging package \pkg{debug}). Thus:

\itemize{
\item \code{use_consolette(TRUE)} will turn the consolette on (and turn off normal debug windows), which is OK if you prefer the consolette anyway in an interactive session)
\item \code{use_consolette(FALSE)} should revert to no
}

You shouldn't close the consolette while actually debugging, i.e. if there are codeframes visible in it. In fact, \code{use_consolette(F)} won't let you, unless you override it with \code{force=TRUE}. Only do that if you're desperate; god knows what state the debugger will be in afterwards.
}
\usage{use_consolette(useit = TRUE, force = FALSE)
}
\arguments{
\item{ useit}{obvious}
\item{ force}{only set to \code{TRUE} if you absolutely must close the consolette while it has e.g. an "orphaned" codeframe still visible. Don't use unless desperate.}
}
\seealso{\code{package?debug}, \code{\link{mrun}}
}
\keyword{misc}
