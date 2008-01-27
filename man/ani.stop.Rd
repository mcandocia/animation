\name{ani.stop}
\alias{ani.stop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Complete the HTML animation page}
\description{
  Complete the HTML animation page created by \code{\link{ani.start}} and restore previous options such as \code{prompt}. 
}
\usage{
ani.stop(footer = TRUE, autobrowse = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{footer}{ logical. If \code{TRUE}, write a foot part in the HTML page containing information such as date/time of creation; else don't write }
  \item{autobrowse}{ logical. Whether auto-browse the animation page immediately after it is completed? }
}
\value{
None (invisible `\code{NULL}').
}
\author{ Yihui Xie }
\note{
If you haven't started an animation page yet, there'll be a warning, and the animation page \emph{will not} be finished unless \code{\link{savePNG}} has been called before.  
}
\seealso{ \code{\link{savePNG}}, \code{\link{ani.start}} }
\examples{
\dontrun{
 
# save the animation in HTML pages and auto-browse it 
ani.start()
boot.iid(saveANI = TRUE, width = 600, height = 500, interval = 0.2)
ani.stop()

}
}
\keyword{ utilities }