\name{step.into.sysfuns
}
\alias{step.into.sysfuns
}
\title{Set or get options for which "special" functions get stepped into, when in step-mode.
}
\description{When the \pkg{debug} package is in step-mode, there are a few special system functions that it can either step into, or leave the system to handle \emph{en bloc}. These functions all have expression-type arguments. Currently, they are: \code{try}, \code{suppressWarnings}, \code{eval}, \code{evalq}, and (for built-in methods only) \code{with} and \code{within}. The step-into behaviour is controlled by calling \code{step.into.sysfuns}, which operates like \code{par}. You can also circumvent step-into at particular lines, by using \code{go(n)} to zoom through to the next statement. Additional methods for \code{with} and \code{within} can be handled via e.g. \code{mtrace( with.myS3class)}, so are not considered "special" here.
}
\usage{
# USAGE is not useful here-- see *Arguments*
step.into.sysfuns( ...)
}
\arguments{\item{ ...}{tag-value pairs of logicals, e.g. \code{with=TRUE, evalq=FALSE}. Legal tags are shown in DESCRIPTION. If empty, return all tags-value pairs, as a logical vector.}
}
\value{Either the previous value(s) of tags that are set, or the entire logical vector of tags.
}
\examples{
step.into.sysfuns() # all of them-- shows which are legal
step.into.sysfuns()['with'] # extract one of them
owith <- step.into.sysfuns( with=FALSE) # don't step into with-statements
step.into.sysfuns( with=owith) # revert to previous
}
\keyword{misc}
