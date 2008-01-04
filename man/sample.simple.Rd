\name{sample.simple}
\alias{sample.simple}
\title{ Demonstration for simple random sampling without replacement}
\description{
  The whole sample frame is denoted by a matrix (nrow * ncol) in the plane just for convenience, and the points being sampled are marked by red circles. Each member of the population has an equal and known chance of being selected. 
}
\usage{
sample.simple(nrow = 10, ncol = 10, size = 15, 
    control = ani.control(interval = 0.2), ...)
}
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
sample.simple()

# HTML animation page
ani.start()
sample.simple(nrow = 5, size = 20, interval = 0.1)
ani.stop() 
}
\keyword{ distribution }
\keyword{ dynamic }