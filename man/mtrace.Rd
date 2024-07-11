\name{mtrace}
\alias{mtrace}
\alias{mtrace.off}
\alias{check.for.tracees}
\alias{tracees}
\title{Interactive debugging}
\description{\code{mtrace} sets or clears debugging mode for a function; \code{mtrace.off} clears debugging mode for all functions; \code{check.for.tracees} shows which functions are in debugging mode.
}
\usage{
# Usual: mtrace( fname) or mtrace( fname, F) or mtrace( apackage:::afunction)
mtrace( fname, tracing=TRUE, char.fname,
    from=mvb.sys.parent(), update.tracees=TRUE, return.envs=FALSE)
mtrace.off()
check.for.tracees( where=1)
}
\arguments{
\item{  fname}{quoted or unquoted function name, or unquoted reference to function in package (via \code{::} or \code{:::}) or list/environment (via \code{$})}
\item{  tracing}{TRUE to turn tracing on, FALSE to turn it off}
\item{  char.fname}{(rarely used) if your function name is stored in a character object \code{x}, use \code{char.fname=x}. If you want to \emph{turn off} tracing while doing so, \code{mtrace( char=x, F)} won't work because of argument matching rules; you need \code{mtrace( char.fname=x, tracing=F)}.}
\item{  from}{where to start looking for \code{fname} (not usually needed)}
\item{  where}{(character or integer) position in search path}
\item{  update.tracees}{don't set this parameter! It's only for use by other functions}
\item{  return.envs}{if TRUE, this will return a list of the environments where the function has been replaced by the \code{mtraced} version}
}
\details{
\code{mtrace(myfun)} modifies the body code of \code{myfun}, and also stores debugging information about \code{myfun} in \code{tracees$myfun}. Next time the function is invoked, the modified debug-ready version will be called instead of the orginal. \code{mtrace} does not modify source code (or other) attributes, so \code{myfun} will "look" exactly the same afterwards. \code{mtrace(myfun,FALSE)} restores \code{myfun} to normal. \code{mtrace.off} unmtraces all mtraced functions (see below for exceptions).

Because \code{mtrace} modifies function bodies (possibly in several places, if namespaced packages are involved), calling \code{save.image} or \code{save} while functions are still \code{mtrace}d is probably not a good idea-- if the saved image is reloaded in a new R{} session, the debugger won't know how to handle the previously \code{mtrace}d functions, and an error message will be given if they are invoked. The \code{Save} and \code{Save.pos} functions in package \pkg{mvbutils} will get round this without your having to manually untrace and retrace functions.

If you do see a "maybe saved before being un-mtraced?" error message when \code{myfun} is invoked, all is not lost; you can restore \code{myfun} to health via \code{mtrace(myfun,F)}, or put it properly into debugging mode via \code{mtrace(myfun)}. \code{mtrace.off} won't work in such cases, because \code{myfun} isn't included in \code{tracees}.

\code{check.for.tracees} checks for functions which have been \code{mtrace}d, but only in one directory. By contrast, \code{names(tracees)} will return all functions that are currently known to be \code{mtrace}d. However, unlike \code{check.for.tracees}, \code{names(tracees)} won't show functions that were saved during a previous R{} session in an \code{mtrace}d state.

\code{mtrace.off} will try to untrace all functions. Specifically, it deals with those returned by \code{names( tracees)} and/or \code{check.for.tracees( 1)}. It doesn't currently deal with methods of reference-class and S4-class objects, for which you'll need to call \code{mtrace(..., tracing=FALSE)} manually.

\code{mtrace} puts a breakpoint (see \code{\link{bp}}) at line 1, but clears all other breakpoints.

\code{mtrace} can handle \code{mlocal} functions, but not (yet) \code{do.in.envir} functions-- the latter appear as monolithic statements in the code window. See package \pkg{mvbutils} for more details.

If you use \code{fixr} to edit functions, \code{mtrace} will automatically be re-applied when an updated function file is sourced back in. Otherwise, you'll need to call \code{mtrace} manually after updating a function.
\subsection{Finding functions}{
\code{mtrace} by default looks for a function in the following \emph{environments}: first in the frame stack, then in the search path, then in all namespaces, then in all S3 methods tables. If several copies of a function are found, all will get modified (mtraced) to the \bold{same} code; ditto when unmtracing.

For functions that live somewhere unusual, you'll need to set the \code{from} argument. One case is for functions that live inside a list, such as family-functions like \code{poisson} for GLMs. In that case, because the list is not an environment, you will have to do something like :

\preformatted{
  funtotrace <- mylist$funtotrace
  mtrace( funtotrace)
  mylist$funtotrace <<- copyfun
}

and a very similar sequence to un-mtrace later. I have thought about ways to automate this, but (i) they are hard to code, and (ii) they might go wrong--- so, it's Do-It-Yourself, please.

Another non-obvious case is as follows. Suppose there is a function \code{f} which first defines functions \code{g} and \code{h}, then calls \code{g}. Now suppose you have \code{mtrace}d \code{f} and then \code{g} from inside \code{f}, and that \code{g} is currently running. If you now want to \code{mtrace(h)}, the problem is that \code{h} is not visible from the frame of \code{g}. To tell \code{mtrace} where to find \code{g}, call \code{mtrace( h, from=sys.parent())}. [You can also replace \code{sys.parent()} with the absolute frame number of \code{f}, if \code{f} has been \code{mtrace}d and its code window is visible.] \code{mtrace} will then look through the enclosing environments of \code{from} until it finds a definition of \code{h}.

If \code{myfun} has been defined in a namespaced package, then there may be several copies of \code{myfun} in the system, different ones being used at different times. \code{mtrace} will change them all; see \code{\link{fun.locator}} if you really want to know more.

If \code{mtrace(bar)} is called while function \code{foo} is being debugged (\code{mtrace(foo)} having previously been called), and \code{bar} has been redefined within \code{foo} or a parent environment of \code{foo}, then only the redefined copy of \code{bar} will be \code{mtrace}d.
}
\subsection{S4 and reference class methods}{
S4 methods can be \code{mtrace}d, but like much about S4 it's clunky; see \code{package?debug}. Reference class methods can be \code{mtrace}d easily after an object has been instantiated. You might call this "object-level" mtracing, because it only works for one object of each class at a time. To \code{mtrace} e.g. the \code{edit} method in the example for "?ReferenceClasses", just do:

\preformatted{
  mtrace( edit, from=xx) # NB will force a method into existence even if it's not been invoked yet
  mtrace( edit, from=xx, FALSE) # to clear it; mtrace.off() won't work properly
}

You can also do "class-level" mtracing, so that all subsequently-created objects of that class will use the \code{mtrace}d version. Just do this:

\preformatted{
  mtrace( edit, from=mEditor$def@refMethods)
  xx <- mEditor$new( ...)
  mtrace( edit, from=mEditor$def@refMethods, FALSE) # to clear it; mtrace.off() won't work properly
}

In the "class-level" case, \code{xx} will still have an \code{mtrace}d version of \code{edit} even after the \code{mtrace( from=mEditor..., FALSE)}. You'll need to use the "object-level" technique to clear it.

As of April 2011, methods are only set up inside a ref-class object when they are first \emph{accessed}, not when the object is created. \code{mtrace} (actually \code{\link{fun.locator}}) works round this.
}
\subsection{Limitations}{
Here's a few; I doubt this is all of them!

A few basic functions (used by the \pkg{debug} package itself) can't be \code{mtrace}d directly--- things go horribly wrong, in a usually obvious way. You have to make a copy with a different name, and \code{mtrace} that instead. I'm not going to try to list them all here!

You can't have \code{mtrace} on simultaneously for two functions that have the same name but that have different bodies and live in different places. In theory, the solution is for me to incorporate "location" into the function-level debug info in \code{mtracees}, but I've not been able to figure out a good general-purpose way to do so. If this describes your particular debugging hell, you certainly have my sympathy...

Functions inside \code{list} objects need to be manually \code{mtrace}d and un-'mtrace'd; see \bold{Finding functions} above.
}
}
\value{
\code{mtrace} by default returns an invisible copy of the modified function body. If you set \code{return.envs=TRUE}, it will instead return a list of the environments in which the function has been modified. This is only intended for "internal use".
\code{check.for.tracees} returns a character vector of function names.
}
\examples{
\dontrun{
mtrace(glm) # turns tracing on
names( tracees) # "glm"
check.for.tracees( "package:base") # "glm"
glm(stupid.args) # voila le debugger
qqq() # back to command prompt
mtrace( glm, FALSE)
mtrace.off() # turns it off for all functions
mtrace( debug:::setup.debug.admin) # woe betide ye
}
}
\keyword{debugging
}
