\name{ani.control}
\alias{ani.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Control for animations }
\description{
  Various parameters that control the behaviour of the animation. 
}
\usage{
ani.control(saveANI = FALSE, interval = 1, nmax = 50, 
    width = 480, height = 480, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saveANI}{ logical. Should the animation frames be saved as png files while being displayed inside R? }
  \item{interval}{ a positive number to set the time interval of the animation (unit in seconds). }
  \item{nmax}{ maximum number of steps for a loop (e.g. iterations) to create animation frames. }
  \item{width}{width of PNG image frames (unit in px); see \code{\link[grDevices:png]{png}}}
  \item{height}{height of PNG image frames (unit in px); see \code{\link[grDevices:png]{png}}}
  \item{\dots}{ other controlling parameters. }
}

\value{
a list containing the options.
}
\author{ Yihui Xie }
\note{ 
Please note that \code{nmax} is usually equal to the number of animation frames (e.g. for \code{\link{brownian.motion}}) but not \emph{always}! The reason is that sometimes there are more than one frame recorded in a single step of a loop, for instance, there are 2 frames generated in each step of \code{\link{kmeans.ani}}, and 4 frames in \code{\link{knn.ani}}, etc. 
}
\seealso{ It is used in many animation functions such as \code{\link{brownian.motion}}, \code{\link{boot.iid}}, \code{\link{buffon.needle}}, \code{\link{cv.ani}}, \code{\link{flip.coin}}, \code{\link{kmeans.ani}}, \code{\link{knn.ani}}, etc.}
\examples{
ani.start() 
op = par(mar = c(3, 3, 2, 0.5), mgp = c(2, .5, 0), tcl = -0.3, 
    cex.axis = 0.8, cex.lab = 0.8, cex.main = 1) 
brownian.motion(control = ani.control(interval = 0, nmax = 100,
    height = 500, width = 600, saveANI = TRUE))
par(op) 
ani.stop() 
}
\keyword{ misc }

