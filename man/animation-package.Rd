\name{animation-package}
\alias{animation-package}
\docType{package}
\title{
Animated Statistics Using R 
}
\description{
Various functions for animations in statistics which could probably aid in teaching statistics and data analysis. 
}
\details{
\tabular{ll}{
Package: \tab animation\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2008-03-10\cr
License: \tab GPL (>=2)\cr
}
This package mainly makes use of HTML \& JavaScript to illustrate animations, and other approaches such as graphical devices (\code{\link[grDevices:windows]{x11}} or \pkg{rgl}, etc) or GIF/MPG movies can also be employed. 
}
\author{
Yihui Xie <\url{http://www.yihui.name}>
}
\references{
Animated Statistis Using R: \url{http://R.yihui.name}; created and maintained by Yihui Xie 
}
\keyword{ package }
\keyword{ dynamic }
\keyword{ device }
\keyword{ dplot } 

\examples{
# create an animation page in the tempdir() and auto-browse it 
# Brownian Motion 
library(animation) 
op = par(mar = c(3, 3, 2, 0.5), mgp = c(2, .5, 0), tcl = -0.3, 
    cex.axis = 0.8, cex.lab = 0.8, cex.main = 1) 
brownian.motion(control = ani.control(interval = 0.05, nmax = 100))
par(op) 

# Bootstrapping
boot.iid(interval = 0.3)
 
}
