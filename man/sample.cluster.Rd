\name{sample.cluster}
\alias{sample.cluster}
\title{ Demonstration for cluster sampling }
\description{
  Every rectangle stands for a cluster, and the simple random sampling without replacement is performed for each cluster. All points in the clusters being sampled will be drawn out.
}
\usage{
sample.cluster(pop = ceiling(10 * runif(10, 0.2, 1)), 
    size = 3, control = ani.control(interval = 0.2), ...)
}
\arguments{
  \item{pop}{ a vector for the size of each cluster in the population. }
  \item{size}{ the number of clusters to be drawn out. }
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
sample.cluster()

\dontrun{ 

# HTML animation page
ani.start()
sample.cluster(pop = sample(15, 5), size = 2, saveANI = TRUE)
ani.stop() 

}
}
\keyword{ distribution }
\keyword{ dynamic }