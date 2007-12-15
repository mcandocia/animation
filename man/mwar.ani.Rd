\name{mwar.ani}
\alias{mwar.ani}
\title{ Demonstration for ``Moving Window Auto-Regression'' }
\description{
  This function just fulfills a very naive idea about moving window regression using rectangles to denote the ``windows'' and move them, and the corresponding AR(1) coefficients as long as rough confidence intervals are computed for data points inside the ``windows'' during the process of moving.  
}
\usage{
mwar.ani(x, k = 15, conf = 2, control = ani.control(), ...)
}
\arguments{
  \item{x}{ univariate time-series (a single numerical vector); default to be \emph{sin(seq(0, 2 * pi, length = 50)) + rnorm(50, sd = 0.2)} }
  \item{k}{ an integer of the window width }
  \item{conf}{ a positive number: the confidence intervals are computed as [ar1 - conf*s.e., ar1 + conf*s.e.] }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
The AR(1) coefficients are computed by \code{\link[stats:arima]{arima}}. 
} 
\value{
A list containing 
  \item{phi }{the AR(1) coefficients}
  \item{L }{lower bound of the confidence interval}
  \item{U }{upper bound of the confidence interval} 
}
\references{ 
Robert A. Meyer, Jr. Estimating coefficients that change over time. \emph{International Economic Review}, 13(3):705-710, 1972.
}
\author{ Yihui Xie }
\seealso{  \code{\link[stats:arima]{arima}} }
\examples{
# moving window along a sin curve 
mwar.ani(interval = 0.3)

# for the data 'pageview'
data(pageview)
mwar.ani(pageview$visits, k = 30, interval = 0.2) 

# HTML animation page
ani.start()
mwar.ani(interval = 0, width = 600, height = 500) 
ani.stop() 
}
\keyword{ dplot }
\keyword{ dynamic }
\keyword{ ts }
