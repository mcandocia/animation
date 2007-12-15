\name{sample.strat}
\alias{sample.strat}
\title{ Demonstration for stratified sampling }
\description{
  Every rectangle stands for a stratum, and the simple random sampling without replacement is performed within each stratum. The points being sampled are marked by red circles. 
}
\usage{
sample.strat(pop = ceiling(10 * runif(10, 0.5, 1)), 
    size = ceiling(pop * runif(length(pop), 0, 0.5)), 
    control = ani.control(interval = 0.2), ...)
}
\arguments{
  \item{pop}{ a vector for the size of each stratum in the population. }
  \item{size}{ a corresponding vector for the sample size in each stratum (recycled if necessary). }
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
sample.strat()

# HTML animation page
ani.start()
sample.strat(pop = sample(15, 5), interval = 0)
ani.stop() 

}
\keyword{ distribution }
\keyword{ dynamic }