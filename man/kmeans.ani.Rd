\name{kmeans.ani}
\alias{kmeans.ani}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstration of K-Means cluster analysis }
\description{
  K-Means cluster algorithm may be regarded as a series of iterations of: finding cluster centers, computing distances between sample points, and redefining cluster membership. This function provides a demo of K-Means cluster algorithm for data containing only two variables (columns). 
}
\usage{
kmeans.ani(saveANI = FALSE, x, centers = 2, interval = 2, nmax = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saveANI}{ logical. Should the animation frames be saved as png files? }
  \item{x}{ A numercal matrix containing only 2 columns.}
  \item{centers}{ The number of clusters; a random set of (distinct) rows in \code{x} is chosen as the initial centers. }
  \item{interval}{ Time interval of the animation (unit in seconds). }
  \item{nmax}{ The maximum number of iterations allowed. See details below. }
}
\details{
  The data given by \code{x} is clustered by the \emph{k}-means method, which aims to partition the points into \emph{k} groups such that the sum of squares from points to the assigned cluster centers is minimized. At the minimum, all cluster centres are at the mean of their Voronoi sets (the set of data points which are nearest to the cluster centre). 

  For practical applications please refer to \code{\link[stats:kmeans]{kmeans}}.
  
  Note that \code{nmax} is defined as the maximum number of iterations in such a sense: an iteration includes the process of computing distances, redefining membership and finding centers. Thus there might be \code{2*nmax} animation frames in the output if \code{saveANI = TRUE}.
}
\value{
  A list with components 
  \item{cluster }{A vector of integers indicating the cluster to which each point is allocated.}
  \item{centers }{A matrix of cluster centers.}
}
\references{ 
Hartigan, J. A. and Wong, M. A. (1979). A K-means clustering algorithm. \emph{Applied Statistics} \bold{28}, 100-108. 
}
\author{ Yihui Xie }
\seealso{ \code{\link[stats:kmeans]{kmeans}} }
\examples{
x = matrix(runif(100), ncol = 2)
kmeans.ani(saveANI = FALSE, x, centers = 2, interval = 1) 

# create HTML animation page 
op = par(mar = c(2, 2, 3, 0), cex.axis = 0.75, cex.main = 1)
ani.start() 
kmeans.ani(saveANI = TRUE, x, centers = 3, interval = 0.1) 
ani.stop(TRUE, TRUE)
par(op)
}
\keyword{ hplot }
\keyword{ dynamic }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ cluster }