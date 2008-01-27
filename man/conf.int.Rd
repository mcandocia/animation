\name{conf.int}
\alias{conf.int}
\title{ Demonstration of Confidence Intervals }
\description{
  This function gives a demonstration of the concept of confidence intervals in mathematical statistics in this way: keep on drawing samples from the Normal distribution N(0, 1), computing the intervals based on a given confidence level and plotting them as segments in a graph. In the end, we may check the coverage rate against the given confidence level. 
}
\usage{
conf.int(level = 0.95, size = 50, 
    control = ani.control(interval = 0.3), ...)
}
\arguments{
  \item{level}{ the confidence level (1 - alpha), e.g. 0.95 }
  \item{size}{ the sample size for drawing samples from N(0, 1) }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  Intervals that cover the true parameter are denoted in gray color, otherwise in red color. As the process of drawing samples goes on, there will be a legend indicating the numbers of the two kinds of intervals respectively. 
  
  The argument \code{nmax} in \code{control} means the times of drawing samples. 
}
\value{
  A list containing 
  \item{level }{confidence level}
  \item{size }{sample size}
  \item{CI}{a matrix of confidence intervals for each sample}
  \item{CR}{coverage rate} 
}
\references{
George Casella and Roger L. Berger. \emph{Statistical Inference}. Duxbury Press, 2th edition, 2001. 
}
\author{ Yihui Xie }
\examples{
conf.int()

\dontrun{
 
# save the animation in HTML pages
ani.start()
conf.int(saveANI = TRUE, interval = 0.2, width = 600, height = 500)
ani.stop() 

}
}
\keyword{ dynamic }
\keyword{ dplot }
\keyword{ distribution }
