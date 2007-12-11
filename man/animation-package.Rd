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
Date: \tab 2007-12-11\cr
License: \tab GPL (>=2)\cr
}
This package mainly makes use of HTML \& JavaScript to illustrate animations, and other approaches such as graphical devices (\code{\link[grDevices:windows]{x11}} or \pkg{rgl}, etc) can also be adopted. 
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
library(animation) 
ani.start() 
brownian.motion(saveANI = TRUE) 
ani.stop() 
}
