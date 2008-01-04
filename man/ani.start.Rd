\name{ani.start}
\alias{ani.start}
\title{ Start the generation of an HTML animation page }
\description{
  Copy JavaScript file \file{FUN.js} and CSS file \file{ANI.css} to the same directory as the HTML animation page and write the header part of the HTML page. The prompt of the current R session is modified (by default \code{ANI>}). 
}
\usage{
ani.start(outdir = tempdir(), filename = "index", extension = "htm", 
    withprompt = "ANI> ", Title = "Animated Statistics Using R")
}
\arguments{
  \item{outdir}{ physical directory to store the animation output }
  \item{filename}{ name of the target HTML main file }
  \item{extension}{ extension of the target HTML file (htm, html,...) }
  \item{withprompt}{ prompt to display while using \code{\link{ani.start}}/\code{\link{ani.stop}} }
  \item{Title}{ string to pass to HTML \code{<title>} tag }
}
\details{
  A new environment called \env{ANIenv} is build, where internal variables such as physical path are stored. This environment is not visible to the user. It is destroyed when calling \code{\link{ani.stop}}. 
  
  Only a part of the HTML page is written (such as the head area), and the rest part will be completed by \code{\link{ani.stop}}. 
}
\value{
None (invisible `\code{NULL}').
}
\author{Yihui Xie}
\note{ 
After calling \code{\link{ani.start}}, either animation functions in this package or R script of your own can be used to generate \& save animated pictures using \code{\link{savePNG}}, then watch your animation by \code{\link{ani.stop}(autobrowse = TRUE)}. 
}
\seealso{ \code{\link{savePNG}}, \code{\link{ani.stop}} }
\examples{
# create an animation page in the tempdir() and auto-browse it 
ani.start() 
brownian.motion(saveANI = TRUE, interval = 0.05) 
ani.stop() 
}
\keyword{ utilities }

