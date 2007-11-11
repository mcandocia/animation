\name{savePNG}
\alias{savePNG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Save PNG image from the current graphical device }
\description{
  Copy the current graphical device to a \acronym{PNG} device to generate a PNG image file. 
}
\usage{
savePNG(outdir = tempdir(), n = 1, width = 480, height = 480, unit = "px", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outdir}{ physical directory to store the PNG image (default in the temporary directory)}
  \item{n}{ file name of the PNG image; can be either numeric or characters }
  \item{width}{ width of PNG image }
  \item{height}{ height of PNG image }
  \item{unit}{ The units in which `height' and `width' are given. Can be `in' (inches), `px' (pixels), `cm' or `mm'. }
  \item{\dots}{ other parameter passed to \code{\link[grDevices:png]{png}} }
}
\details{
  Actually for Windows users, \code{savePNG} is almost identical to \code{\link[grDevices:savePlot]{savePlot}(type = "png")}. However, there's no \code{savePlot} for Linux users, so this special funtion just acts as a wrapper for saving plots. The work behind this function is just to copy the plot in the current graphical device to a png device using \code{\link[grDevices:dev2]{dev.copy}}. 
  
  To make animations, we need to specify the argument \code{n} as a sequence of integers so that we can ultimately get a series of image files named \file{1.png}, \file{2.png}, ... Thus the JavaScript in the HTML animation page will be able to find these image frames. Check the souce code of any animation functions to understand this approach. 
}
\value{
None (invisible `\code{NULL}').
}

\author{ Yihui Xie }
\note{ 
This function provides a \emph{general} method to save plots as PNG files, thus \code{\link{ani.start}} does not need to be called beforehand unless you want to make animations.
}
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
  # a plot save to file.path(getwd(), "1.png") 
  plot(rnorm(100)) 
  savePNG(getwd(), n = 1)
  
  # a sequence of plots saved in the work directory 
  for(i in 1:10){
    plot(rnorm(1), rnorm(1), xlim = c(-1, 1), ylim = c(-1, 1))
    savePNG(getwd(), n = i) 
  } 
}
\keyword{ device }