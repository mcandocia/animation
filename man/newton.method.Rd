\name{newton.method}
\alias{newton.method}
\title{ Demonstration of the Newton-Raphson Method for Root-finding }
\description{
  Newton's method (also known as the Newton-Raphson method or the Newton-Fourier method) is an efficient algorithm for finding approximations to the zeros (or roots) of a real-valued function f(x). This function provides an illustration of the iterations in Newton's method. 
}
\usage{
newton.method(FUN = function(x) x^2 - 4, init = 10, 
    rg = c(-1, 10), tol = 0.001, interact = FALSE, 
    control = ani.control(interval = 2), ...)
}
\arguments{
  \item{FUN}{ the function in the equation to solve (univariate) }
  \item{init}{ the starting point }
  \item{rg}{ the range for plotting the curve }
  \item{tol}{ the desired accuracy (convergence tolerance) }
  \item{interact}{ logical; whether choose the starting point by cliking on the curve (for 1 time) directly? }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  The iteration goes on in this way:
  
  \deqn{x_{k + 1} = x_{k} - \frac{FUN(x_{k})}{FUN'(x_{k})}}{x[k + 1] = x[k] - FUN(x[k]) / FUN'(x[k])} 

  From the starting value x0, blue vertical lines and red points are plotted to show the location of the sequence of iteration values x1, x2, ...; red tangent lines are drawn to illustrate the relationship between successive iterations; the iteration values are in the right margin of the plot. 
}
\value{
  A list containing 
  \item{root }{the root found by the algorithm}
  \item{value }{the value of \code{FUN(root)}}
  \item{iter}{number of iterations; if it is equal to \code{control$nmax}, it's quite likely that the root is not reliable because the maximum number of iterations has been reached} 
}
\note{
The algorithm might not converge -- it depends on the starting value. See the examples below. 
} 
\references{ 
\url{http://en.wikipedia.org/wiki/Newton's_method}
}
\author{ Yihui Xie }
\seealso{ \code{\link[stats]{optim}} }
\examples{
op = par(pch = 19) 
# default example 
xx = newton.method() 
xx$root  # solution

\dontrun{
 
# another function 
xx = newton.method(function(x) exp(-x) * x, rg = c(0, 
    10), init = 2) 
# not converge!
xx = newton.method(function(x) atan(x), rg = c(-5, 
    5), init = 1.5) 
xx$root   # Inf 
# interaction: use your mouse to select the starting point
xx = newton.method(function(x) atan(x), rg = c(-2, 
    2), interact = TRUE) 

# HTML animation pages 
ani.start()
newton.method(function(x) exp(-x) * x, rg = c(0, 10), 
    init = 2, saveANI = TRUE, width = 600, height = 500)
ani.stop() 

}
par(op) 
}
\keyword{ optimize }
\keyword{ dynamic }
\keyword{ dplot }
