\name{saveMovie}
\alias{saveMovie}
\title{ Convert Images to A Single Animated Movie }
\description{
  This function opens a graphical device first to generate a sequence of images, then makes use of the command convert in `ImageMagick' to convert these images to a single animated movie (in formats such as GIF and MPG, etc). 
}
\usage{
saveMovie(expr, interval = 1, moviename = "movie", movietype = "gif", 
    loop = 0, dev = png, filename = "Rplot", fmt = "\%03d", 
    outdir = tempdir(), ...)
}
\arguments{
  \item{expr}{ an expression to generate animations; use either the animation functions (e.g. \code{brownian.motion(nmax = 30)}) in this package or a custom expression (e.g. \code{for(i in 1:10) plot(runif(10), ylim = 0:1)}). }
  \item{interval}{ duration between animation frames (unit in seconds) }
  \item{moviename}{ file name of the movie (base only, without extension) }
  \item{movietype}{ format of the movie ('gif', 'mpg', ...; as long as it's supported by ImageMagick) }
  \item{loop}{ iterations of the movie; set iterations to zero to repeat the animation an infinite number of times, otherwise the animation repeats itself up to \code{loop} times (N.B. for GIF only!) } 
  \item{dev}{ a function for a graphical device such as \code{\link[grDevices:png]{png}}, \code{\link[grDevices:png]{jpeg}} and \code{\link[grDevices:png]{bmp}}, etc. }
  \item{filename}{ file name of the sequence of images (`pure' name; without any format or extension) }
  \item{fmt}{ a C-style string formatting command, such as \code{'\%3d'}  }
  \item{outdir}{ the directory for the movie frames and the movie itself }
  \item{\dots}{ other arguments passed to the graphical device, such as \code{height} and \code{width}, ... }
}
\details{
  The convenience of this function is that it can create a single movie file, however, two drawbacks are obvious too: (1) we need a special (free) software ImageMagick; (2) the speed of the animation cannot be conveniently controlled, as we have specified a fixed \code{interval}. So just go ahead with this function to create your movies if you don't mind these two points. 
}
\value{
An integer indicating failure (-1) or success (0) of the converting (refer to \code{\link[base]{shell}}). 
}
\references{
http://www.imagemagick.org/script/convert.php 
}
\author{ Yihui Xie }
\note{ 
Creating HTML animation pages will not require any other software to be installed, and the duration of animations can also be controlled; refer to \code{\link{ani.start}} and \code{\link{ani.stop}}. 
}
\seealso{ \code{\link[base]{shell}}, \code{\link{ani.start}}, \code{\link{ani.stop}}, \code{\link[grDevices:png]{png}} }
\examples{
## make sure ImageMagick has been installed in your system 
\dontrun{

saveMovie(for(i in 1:10) plot(runif(10), ylim = 0:1), loop = 1)
saveMovie(brownian.motion(nmax = 40), width = 600, height = 600) 

} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dynamic }
\keyword{ device }
\keyword{ utilities }
