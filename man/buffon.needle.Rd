\name{buffon.needle}
\alias{buffon.needle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Simulation of Buffon's Needle }
\description{
  This function provides a simulation for the problem of Buffon's Needle, which is one of the oldest problems in the field of geometrical probability. `Needles' are denoted by segments in the plane, and dropped randomly to check whether they cross the lines. Through many times of `dropping' needles, the approximate value of pi can be calculated out. 
}
\usage{
buffon.needle(saveANI = FALSE, l = 0.8, d = 1, interval = 0.05, 
  nmax = 100, redraw = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saveANI}{ logical. Should the animation frames be saved as png files? }
  \item{l}{ numerical. length of the needle. }
  \item{d}{ numerical. distances between lines. }
  \item{interval}{ time interval of the animation (unit in seconds), i.e. duration between drops. }
  \item{nmax}{ maximum number of simulations (drops). }
  \item{redraw}{ logical. redraw former `needles' or not for each drop. }
}
\details{
  This is quite an old problem in probability. For mathematical background, please refer to \url{http://en.wikipedia.org/wiki/Buffon's_needle} or \url{http://www.mste.uiuc.edu/reese/buffon/buffon.html}.
 
}
\value{
The values of estimated pi are returned as a numerical vector (of length \code{nmax}). 
}
\references{
Ramaley, J. F. (Oct 1969). Buffon's Noodle Problem. \emph{The American Mathematical Monthly} \bold{76} (8): 916-918. 
}
\author{ Yihui Xie }
\note{ 
Note that \code{redraw} will affect the speed of the simulation (animation) to a great deal if \code{nmax} is quite large, so you'd better specify it as \code{FALSE} when doing large amout of simulations. 
}
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
# it takes several seconds if 'redraw = TRUE' 
buffon.needle(nmax = 500, interval = 0)

# this will be faster 
buffon.needle(nmax = 500, interval = 0, redraw = FALSE)

# create HTML animation page
ani.start()
buffon.needle(TRUE, nmax = 100, interval = 0.01, redraw = FALSE) 
ani.stop(autobrowse = TRUE) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
\keyword{ hplot }
\keyword{ dynamic }
\keyword{ math }

