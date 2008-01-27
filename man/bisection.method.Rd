\name{bisection.method}
\alias{bisection.method}
\title{ Demonstration of the Bisection Method for Root-finding on an Interval }
\description{
  In mathematics, the bisection method is a root-finding algorithm which works by repeatedly dividing an interval in half and then selecting the subinterval in which a root exists. This function gives a visual demonstration of this process of finding the root of an equation f(x) = 0. 
}
\usage{
bisection.method(FUN = function(x) x^2 - 4, rg = c(-1, 10), 
    tol = 0.001, interact = FALSE, control = ani.control(), ...)
}
\arguments{
  \item{FUN}{ the function in the equation to solve (univariate)}
  \item{rg}{ a vector containing the end-points of the interval to be searched for the root; in a \code{c(a, b)} form  }
  \item{tol}{ the desired accuracy (convergence tolerance) }
  \item{interact}{ logical; whether choose the end-points by cliking on the curve (for two times) directly? }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  Suppose we want to solve the equation f(x) = 0. Given two points a and b such that f(a) and f(b) have opposite signs, we know by the intermediate value theorem that f must have at least one root in the interval [a, b] as long as f is continuous on this interval. The bisection method divides the interval in two by computing c = (a + b) / 2. There are now two possibilities: either f(a) and f(c) have opposite signs, or f(c) and f(b) have opposite signs. The bisection algorithm is then applied recursively to the sub-interval where the sign change occurs.
  
  During the process of searching, the mid-point of subintervals are annotated in the graph by both texts and blue straight lines, and the end-points are denoted in dashed red lines. The root of each iteration is also plotted in the right margin of the graph. 
}
\value{
  A list containing 
  \item{root }{the root found by the algorithm}
  \item{value }{the value of \code{FUN(root)}}
  \item{iter}{number of iterations; if it is equal to \code{control$nmax}, it's quite likely that the root is not reliable because the maximum number of iterations has been reached} 
}
\references{ 
\url{http://en.wikipedia.org/wiki/Bisection_method} 
}
\author{ Yihui Xie }
\seealso{ \code{\link[stats]{deriv}}, \code{\link[stats]{uniroot}} }
\examples{
# default example 
xx = bisection.method() 
xx$root  # solution

\dontrun{
 
# a cubic curve 
f = function(x) x^3 - 7 * x - 10 
xx = bisection.method(f, c(-3, 5)) 
# interaction: use your mouse to select the end-points 
bisection.method(f, c(-3, 5), interact = TRUE) 

# HTML animation pages 
ani.start()
bisection.method(saveANI = TRUE, width = 600, height = 500)
ani.stop() 

}
}
\keyword{ optimize }
\keyword{ dynamic }
\keyword{ dplot }
