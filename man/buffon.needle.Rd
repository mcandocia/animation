\name{buffon.needle}
\alias{buffon.needle}
\title{ Simulation of Buffon's Needle }
\description{
  This function provides a simulation for the problem of Buffon's Needle, which is one of the oldest problems in the field of geometrical probability. `Needles' are denoted by segments in the plane, and dropped randomly to check whether they cross the lines. Through many times of `dropping' needles, the approximate value of \code{pi} can be calculated out. 
}
\usage{
buffon.needle(l = 0.8, d = 1, redraw = TRUE, 
    control = ani.control(interval = 0.05, nmax = 100), ...)
}
\arguments{
  \item{l}{ numerical. length of the needle; shorter than \code{d}. }
  \item{d}{ numerical. distances between lines; it should be longer than \code{l}. }
  \item{redraw}{ logical. redraw former `needles' or not for each drop. }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}}}
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
Note that \code{redraw} will affect the speed of the simulation (animation) to a great deal if the control argument \code{nmax} (in \code{control}) is quite large, so you'd better specify it as \code{FALSE} when doing large amount of simulations. 
}
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
# it takes several seconds if 'redraw = TRUE' 
buffon.needle(nmax = 500, interval = 0)

# this will be faster 
buffon.needle(nmax = 500, interval = 0, redraw = FALSE)

\dontrun{
 
# create HTML animation page
ani.start()
buffon.needle(saveANI = TRUE, nmax = 100, interval = 0, 
    width = 600, height = 600, redraw = FALSE) 
ani.stop() 

}
}
\keyword{ dplot }
\keyword{ hplot }
\keyword{ dynamic }
\keyword{ math }

