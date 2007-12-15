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
Date: \tab 2007-12-12\cr
License: \tab GPL (>=2)\cr
}
This package mainly makes use of HTML \& JavaScript to illustrate animations, and other approaches such as graphical devices (\code{\link[grDevices:windows]{x11}} or \pkg{rgl}, etc) can also be employed. 
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
ani.start() 
op = par(mar = c(3, 3, 2, 0.5), mgp = c(2, .5, 0), tcl = -0.3, 
    cex.axis = 0.8, cex.lab = 0.8, cex.main = 1) 
brownian.motion(control = ani.control(saveANI = TRUE, interval = 0.01,
    nmax = 100))
par(op) 
ani.stop() 

# Bootstrapping
ani.start()
boot.iid(saveANI = TRUE, width = 600, height = 500, interval = 0)
ani.stop()
 
}
