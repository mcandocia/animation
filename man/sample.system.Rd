\name{sample.system}
\alias{sample.system}
\title{ Demonstration for systematic sampling }
\description{
  The whole sample frame is denoted by a matrix (nrow * ncol) in the plane, and the sample points with equal intervals are drawn out according to a random starting point. The points being sampled are marked by red circles.
}
\usage{
sample.system(nrow = 10, ncol = 10, size = 15, 
    control = ani.control(interval = 0.2), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrow}{ the desired number of rows of the sample frame. }
  \item{ncol}{ the desired number of columns of the sample frame. }
  \item{size}{ the sample size. }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}}}
}
\value{
None (invisible `\code{NULL}').
}
\references{
Cochran, W G (1977) \emph{Sampling Techniques}, Wiley, ISBN 0-471-16240-X 
}
\author{ Yihui Xie }
\seealso{ \code{\link[base:sample]{sample}} }
\examples{
sample.system()

# HTML animation page
ani.start()
sample.system(nrow = 5, size = 12, interval = 0)
ani.stop() 
}
\keyword{ distribution }
\keyword{ dynamic }