\name{g.brownian.motion}
\alias{g.brownian.motion}
\title{Brownian Motion Using Google Visualization API...}
\usage{g.brownian.motion(p=20, start=1900, digits=14,
    file="brownian.motion.html", width=800, height=600)}
\description{Brownian Motion Using Google Visualization API
We can use R to generate random numbers from the Normal distribution and
write them into an HTML document, then the Google Visualization gadget
``motionchart'' will prepare the animation for us (a Flash animation with
several buttons).}
\value{\code{NULL}. An HTML page will be opened as the side effect.}
\note{The number of frames is controlled by \code{ani.options("nmax")} as
usual.}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{brownian.motion}}, \code{\link{BM.circle}},
\code{\link[stats]{rnorm}}}
\references{\url{http://code.google.com/apis/visualization/}

\url{http://yihui.name/en/2008/11/brownian-motion-using-google-visualization-api-and-r/}}
\keyword{dynamic}
\keyword{IO}
\arguments{\item{p}{number of points}
\item{start}{start "year"; it has no practical meaning in this animation
but it's the required by the Google gadget}
\item{digits}{the precision to round the numbers}
\item{file}{the file name}
\item{width,height}{width and height of the animation}}
\examples{\dontrun{
g.brownian.motion(15, digits = 2, width = 600, 
height = 500) 
}}
