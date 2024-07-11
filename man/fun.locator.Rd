\name{fun.locator}
\alias{fun.locator}
\title{Get environment(s) where an object exists.}
\description{Checks the frame stack, the search list, any namespaces, and any S3 method tables for copies of an object (normally a function). Used by \code{\link{mtrace}}. If the search starts from a reference-class object, only the object itself is searched, and legit methods are forced into existence.
}
\usage{fun.locator( fname, from=.GlobalEnv, mode='function')
}
\arguments{
\item{ fname}{character string (the object's name)}
\item{ from}{Where to start looking:either an environment, another function (in which case, that function's defining environment is used), or a frame number. 0 is treated as \code{.GlobalEnv}. \code{from} \emph{can} also be a \code{list} object, which will be returned if it contains \code{fname}; however, the result may not be of much use (e.g. \code{\link{mtrace}} can't use it directly).}
\item{ mode}{What type of object to look for-- by default, a function. Set to \code{any} if the object's mode is not relevant.}
}
\value{A list of environments where \code{fname} was found, of length 0 if no copies were found. First, any environments between \code{from} and \code{topenv( from)} are searched (normally, this means temporary frames wherein \code{fname} could have been defined; but see \bold{Details}). Next, the search list is checked. Next, all loaded namespaces are checked. Finally, all S3 method tables are checked.
}
\details{
When \code{fname} is defined in a namespaced package \pkg{foo}, several copies will exist at once. If \code{foo} exports \code{fname}, then \code{fname} will be found both in the search path (in \code{package:foo}) and in the "hidden" environment \code{asNamespace( "foo")}. The first version will be found by "normal" calls to \code{fname}, but the hidden version will be found whenever \code{fname} is invoked by a function in \code{package:foo}. If the S3 method \code{somegeneric.fname} is defined in \code{package:foo}, then it will exist both in \code{asNamespace("foo")} and in \code{get( ".__S3MethodsTable__.", asNamespace( "foo"))}, although not in the main search path. If \code{fname} is both exported and an S3 method, then there will be three copies. Other packages that import from package \pkg{foo} may also have \code{fname} in the parent environments of their namespaces. All such duplicates should probably be identical, and if you want to change any one copy, you probably want to change all of them.

Normally, the search path environment(s) where \code{fname} is found will be given before (i.e. with lower index than) the hidden namespace environment. However, if \code{from} is used to start the search in a hidden namespace environment where \code{fname} exists, then the hidden namespace will be returned first. Duplicated environments are removed from the return list.

For reference-class and S4 methods, you need to set \code{from} explicitly; see \code{?mtrace} and \code{package?debug} respectively.

For functions within \code{list}s (as opposed to existing directly within an \code{environment}), \code{\link{mtrace}} needs you to mess about manually; see its \code{FINDING FUNCTIONS} subsection.
}
\seealso{\code{getAnywhere}
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities
}
