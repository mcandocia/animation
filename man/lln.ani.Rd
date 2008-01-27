\name{lln.ani}
\alias{lln.ani}
\title{ Demonstration of Law of Large Numbers }
\description{
  This function plots the sample mean as the sample size grows to check whether the sample mean approaches to the population mean. 
}
\usage{
lln.ani(FUN = rnorm, mu = 0, np = 30, pch = 20, 
    control = ani.control(interval = 0.3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FUN}{ a function to generate random numbers from a certain distribution }
  \item{mu}{ population mean; passed to \code{FUN} }
  \item{np}{ times for sampling from a distribution (not the sample size!); to examine the behaviour of the sample mean, we need more times of sampling to get a series of mean values }
  \item{pch}{ symbols for points; see Details }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  \code{np} points are plotted to denote the distribution of the sample mean; we will observe that the range of the sample mean just becomes smaller and smaller as the sample size increases and ultimately there will be an obvious trend that the sample mean converges to the population mean \code{mu}. 
  
  The argument \code{nmax} in \code{control} means the maximum sample size.
}
\note{
  The argument \code{pch} will influence the speed of plotting, and for a very large sample size (say, 300), it is suggested that this argument be specified as '\code{.}'. 
} 
\value{
None (invisible `\code{NULL}').
}
\references{ 
George Casella and Roger L. Berger. \emph{Statistical Inference}. Duxbury Press, 2th edition, 2001. 
}
\author{ Yihui Xie }
\examples{
lln.ani()

\dontrun{
 
# save the animation in HTML pages
ani.start()
lln.ani(saveANI = TRUE, interval = 0.2, width = 600, height = 500)
ani.stop() 

}
}
\keyword{ dynamic }
\keyword{ dplot }
\keyword{ distribution }
