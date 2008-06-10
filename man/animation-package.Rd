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
Version: \tab 1.0\cr
Date: \tab 2008-06-10\cr
License: \tab GPL (>=2)\cr
}
This package mainly makes use of HTML \& JavaScript to illustrate animations, and other approaches such as windows graphics devices (\code{\link[grDevices:windows]{x11}} or \pkg{rgl}, etc) or GIF/MPG movies can also be employed.
}
\author{
Yihui Xie <\url{http://www.yihui.name}>
}
\references{
AniWiki: Animations in Statistics \url{http://animation.yihui.name}; created and maintained by Yihui Xie
}
\keyword{ package }
\keyword{ dynamic }
\keyword{ device }
\keyword{ dplot } 

\examples{
# create an animation page in the tempdir() and auto-browse it 
# Brownian Motion 
oopt = ani.options(interval = 0.05, nmax = 100, ani.dev = png, ani.type = "png")
ani.start()
opar = par(mar = c(3, 3, 2, 0.5), mgp = c(2, .5, 0), tcl = -0.3,
    cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
brownian.motion( pch = 21, cex = 5, col = "red", bg = "yellow",
    main = "Demonstration of Brownian Motion",)
par(opar)
ani.stop()
ani.options(oopt)

# Bootstrapping
ani.options(interval = 0.3)
boot.iid()
 
}
