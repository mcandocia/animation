\name{boot.iid}
\alias{boot.iid}
\title{ Bootstrap for i.i.d data }
\description{
  Demonstrate bootstrapping for i.i.d data: use sunflower scatter plot to illustrate the situation of sampling, and histogram to show the distribution of the statistic of interest. 
}
\usage{
boot.iid(x = runif(20), statistic = mean, m = length(x), 
    control = ani.control(), ...)
}
\arguments{
  \item{x}{ a numerical vector (the original data). }
  \item{statistic}{ A function which returns a value of the statistic of interest when applied to the data x. }
  \item{m}{ the sample size for bootstrapping (m-out-of-n bootstrap) }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  This is actually a very naive version of bootstrapping but may be useful for novices. The blue points denote the orignial dataset, while the red points with (possible) leaves denote sample points being resampled; the number of leaves just means how many times these points are resampled, as bootstrap samples \emph{with} replacement. 
}
\value{
  A list containing
  \item{t0 }{ The observed value of 'statistic' applied to 'x'.}
  \item{tstar }{Bootstrap versions of the 'statistic'.}
}
\references{ 
There are many references explaining the bootstrap and its variations. For a relatively complete one, you may just refer to:

Efron, B. and Tibshirani, R. (1993) \emph{An Introduction to the Bootstrap}. Chapman \& Hall.

}
\author{ Yihui Xie }
\seealso{ \code{\link[graphics:sunflowerplot]{sunflowerplot }}, \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
# bootstrap for 20 random numbers following U(0, 1) 
boot.iid(interval = 0.5)

# for the median of 15 points from chi-square(5) 
boot.iid(x = rchisq(15, 5), statistic = median, interval = 0.5) 

\dontrun{
 
# save the animation in HTML pages
ani.start()
boot.iid(saveANI = TRUE, width = 600, height = 500, interval = 0)
ani.stop()

}
}
\keyword{ nonparametric }
\keyword{ dynamic }
\keyword{ dplot }

