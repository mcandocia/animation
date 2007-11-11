\name{brownian.motion}
\alias{brownian.motion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstration of Brownian motion on the 2D plane }
\description{
  Demonstrate Brownian motion (random walk) in a 2D scatterplot. 
}
\usage{
brownian.motion(saveANI = FALSE, n = 10, interval = 0.05, nmax = 100, 
  main = "Demonstration of Brownian Motion", xlim = c(-20, 20), 
  ylim = c(-20, 20), pch = 21, cex = 5, col = "red", bg = "yellow", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saveANI}{ logical. Should the animation frames be saved as png files? }
  \item{n}{ Number of points in the scatterplot }
  \item{interval}{ time interval of the animation (unit in seconds) }
  \item{nmax}{ maximum number of animation frames (i.e. steps of the motion) }
  \item{main, xlim, ylim, pch, cex, col, bg}{ Arguments passed to \code{\link[graphics:plotdefault]{plot.default}} to control the apperance of the scatterplot (title, points, etc), see \code{\link[graphics:points]{points}} for details.}
  \item{\dots}{ other graphical parameters passed to \code{\link[graphics:plotdefault]{plot.default}}}
}
\details{
  Brownian motion, or random walk, can be regarded as the trace of some cumulative normal random numbers: the location of the next step is just ``current location + random Gaussian numbers'', i.e.,
  
  \deqn{x[k + 1] = x[k] + rnorm(1)\cr 
  y[k + 1] = y[k] + rnorm(1)} 

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
  brownian.motion(n = 15, nmax = 100) 
}
\keyword{ graphs }
\keyword{ dynamic }% __ONLY ONE__ keyword per line