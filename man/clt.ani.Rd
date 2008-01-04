\name{clt.ani}
\alias{clt.ani}
\title{ Demonstration for the Central Limit Theorem }
\description{
  First of all, \code{n} sample points are generated from a certain distribution, and the ECDF as well as the PDF (estimated from \code{\link[stats:density]{density}}) is plotted. Then the sample mean is computed, and at last the density of these sample means is plotted as the repeating of generation of random numbers from that distribution. 
}
\usage{
clt.ani(n = 30, FUN = runif, control = ani.control(interval = 0.1), 
    ...)
}
\arguments{
  \item{n}{ the number of sample points to be generated from the distribution }
  \item{FUN}{ the function to generate \code{n} random numbers from a certain distribution }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  As long as the conditions of the Central Limit Theorem (CLT) are satisfied, the distribution of the sample mean will be approximate to the Normal distribution as long as \code{n} is large enough no matter what is the original distribution. 
}
\value{
None.
}
\references{ 
E. L. Lehmann, \emph{Elements of Large-Sample Theory}. Springer-Verlag, New York, 1999.
}
\author{ Yihui Xie }
\seealso{ \code{\link[stats:density]{density}} }
\examples{
clt.ani()

# HTML animation page
ani.start()
clt.ani(saveANI = TRUE, height = 500, width = 600)
ani.stop()

# other distributions: Chi-square with df = 5 
f = function(n) rchisq(n, 5) 
clt.ani(FUN = f) 
}
\keyword{ dynamic }
\keyword{ distribution }
\keyword{ dplot }