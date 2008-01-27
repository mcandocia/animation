\name{brownian.motion}
\alias{brownian.motion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstration of Brownian motion on the 2D plane }
\description{
  Demonstrate Brownian motion (random walk) in a 2D scatterplot. 
}
\usage{
brownian.motion(n = 10, main = "Demonstration of Brownian Motion", 
    xlim = c(-20, 20), ylim = c(-20, 20), pch = 21, cex = 5, 
    col = "red", bg = "yellow", control = ani.control(nmax = 100, 
    interval = 0.05), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ Number of points in the scatterplot }
  \item{main, xlim, ylim, pch, cex, col, bg}{ Arguments passed to \code{\link[graphics:plotdefault]{plot.default}} to control the apperance of the scatterplot (title, points, etc), see \code{\link[graphics:points]{points}} for details.}
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}}}
}
\details{
  Brownian motion, or random walk, can be regarded as the trace of some cumulative normal random numbers: the location of the next step is just ``current location + random Gaussian numbers'', i.e.,
  
  \deqn{x_{k + 1} = x_{k} + rnorm(1);\;\;  
  y_{k + 1} = y_{k} + rnorm(1)}{x[k + 1] = x[k] + rnorm(1)\cr y[k + 1] = y[k] + rnorm(1)} 
  where \emph{(x, y)} stands for the location of a point. 
}
\value{
None (invisible `\code{NULL}').
}

\author{ Yihui Xie}
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
# show an animation in (Windows/X Window...) a graphics device 
# unless you have opened an invisible device like png(), pdf(), ... 
brownian.motion(n = 15, nmax = 150) 

\dontrun{
 
# create HTML animation page
ani.start()
brownian.motion(n = 15, nmax = 100, saveANI = TRUE, width = 500,
    height = 500) 
ani.stop() 

}
}
\keyword{ dplot }
\keyword{ dynamic }