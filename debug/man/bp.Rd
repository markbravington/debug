\name{bp}
\alias{bp}
\title{Breakpoints for debugging}
\description{Sets/clears breakpoints (including conditional) breakpoints in functions that have been \code{\link{mtrace}}d for debugging.
}
\usage{bp( line.no, expr=TRUE, fname) # fname rarely needed
}
\arguments{
\item{ line.no}{line number}
\item{ expr}{unquoted expression to be tested when execution reaches \code{line.no}}
\item{ fname}{name of function to twiddle breakpoints in}
}
\details{
Breakpoints can only be set after \code{\link{mtrace}}ing a function, and are normally set while the function is actually being debugged. The simplest way is to look at the code window to identify which lines to stop at, call \code{bp(n)} for each of those lines, and then call \code{go()} to enter go mode. Your function code will then be executed without pausing for input until a breakpoint is triggered (or an error occurs, or the function finishes normally). To clear a breakpoint for line \code{n}, type \code{bp(n,FALSE)}.

All line-numbered statements actually have an associated breakpoint expression. When the debugger reaches a line-numbered statement, it evaluates the corresponding breakpoint expression in the function's frame. If the result is not identical to \code{FALSE}, the breakpoint is triggered. By default, all statements have their breakpoint expressions set to \code{FALSE} (by \code{\link{mtrace}}), except for line 1 where the expression is set to TRUE.

After setting a breakpoint for line \code{n}, you will see an asterisk (*) in line \code{n} of the left-hand column of the code window. The asterisk is shown whenever the unevaluated breakpoint expression is not identical to \code{FALSE}.

Conditional breakpoints are just expressions other than \code{TRUE} or \code{FALSE}. To get the debugger to stop at line 5 whenever \code{a} is greater than \code{b}, type \code{bp( 5, a>b)}-- don't \code{quote()} the breakpoint expression. Any statement, including a braced statement, can be used, and the debugger will only pause if the result is not \code{FALSE}. You can therefore use "non-breaking breakpoints" to patch expressions into the code. For instance, if you realize that you should have inserted the statement \code{a <- a+1} just before line 7 of your code, you can type \code{bp( 7, { a <- a+1; FALSE})}; when the debugger reaches line 7, it will increment \code{a} but will not switch to step mode, because the overall result was \code{FALSE}.

Sometimes it is useful to clear the line 1 breakpoint before invoking a function, especially if the function is being called repeatedly. The debugger actually starts in go mode, and does not display a code window or pause for input until a breakpoint is triggered; so if the line 1 breakpoint is cleared, execution can continue at full speed until an error occurs (or another breakpoint is triggered). To adjust breakpoints before a function is invoked, you will need to use the \code{fname} argument. To set/clear breakpoints in function \code{f} at lines other than 1, first type \code{tracees$f$line.list} to see which line numbers correspond to which statements.

Breakpoints in body code apply "globally" to all incarnations of a function, and will be retained when the debugger finishes and the R{} prompt returns. Breakpoint expressions for \code{f} will be saved in \code{tracees$f$breakpoints}.

Breakpoints can be set in \code{on.exit} code as well (but are specific to the incarnation they are set in). It is often useful to set a breakpoint at the first exit code statement (which will be \code{NULL} if \code{on.exit} has not yet been called); this has the effect of a "run-until-finished-then-pause" breakpoint. Whenever \code{on.exit} is called, any existing exit code breakpoints are lost; but if any were present, a new unconditional breakpoint is set at the start of the exit code.

Breakpoints are evaluated in step mode too, but the debugger remains in step mode whatever the result.

At present, all breakpoints are destroyed when functions are edited; if you use \code{fixr}, \code{\link{mtrace}} will be re-applied automatically, but breakpoints will be lost. However, my S+ versions of \code{debug} and \code{mvbutils} used to make an effort to preserve breakpoints across edits, and I plan to introduce something similar in R{}. (The documentation has said "I plan" for about 15 years now... so I guess I don't really plan to, after all.)
}
\seealso{\code{\link{mtrace}}, \code{\link{go}}
}
\examples{
\dontrun{
mtrace( glm)
glm( 35)
# Once the debugger starts:
bp(7) # unconditional breakpoint at line 7
bp(7,F) # to clear it.
bp(7,x>1) # conditional; will trigger if "x>1" (or if "x>1" causes error)
bp(1,F,"glm") # can be called BEFORE debugging glm (but after mtrace);
# ... prevents debugger from halting at start of function
qqq() # exit debugger
mtrace.off()
}
}
\author{Mark Bravington}
\keyword{debugging
}
