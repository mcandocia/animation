\name{vi.lilac.chaser}
\alias{vi.lilac.chaser}
\title{ Visual Illusions: Lilac Chaser }
\description{
  Stare at the center cross for a few (say 30) seconds to experience the phenomena of the illusion.
}
\usage{
vi.lilac.chaser(np = 16, col = "magenta", bg = "gray", cex.p = 7, 
    cex.c = 5, control = ani.control(interval = 0.05, nmax = 30), ...)
}
\arguments{
  \item{np}{ number of points }
  \item{col}{ color of points }
  \item{bg}{ background color of the plot }
  \item{cex.p}{ magnification of points }
  \item{cex.c}{ magnification of the center cross }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}}}
}
\details{
  Just try it out. 
}
\value{
None.
}
\references{ \url{http://en.wikipedia.org/wiki/Lilac_chaser} }
\author{ Yihui Xie }
\note{ 
Points in the original version of `Lilac Chaser' \emph{blurred}, which is not implemented in this function. If you have any idea, please contact me. 
}
\seealso{ \code{\link[graphics:points]{points}} }
\examples{
op = par(pty = "s") 
vi.lilac.chaser(nmax = 20) 

# HTML animation page; nmax = 1 is enough! 
ani.start()
vi.lilac.chaser(saveANI = TRUE, nmax = 1) 
ani.stop() 
}
\keyword{ dynamic }

