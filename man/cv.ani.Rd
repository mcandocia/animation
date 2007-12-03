\name{cv.ani}
\alias{cv.ani}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstration for the process of cross-validation }
\description{
  Simply speaking, the process of cross-validation is just to split the whole data set into several parts and select one part as the test set and the rest parts as the training set. This function uses rectangles to illustrate these `parts' and mark the test set \& the training set with different colors. 
}
\usage{
cv.ani(saveANI = FALSE, x = runif(150), k = 10, interval = 2, 
  nmax = 50, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saveANI}{ logical. Should the animation frames be saved as png files? }
  \item{x}{ a numerical vector which stands for the sample points.}
  \item{k}{ an integer: how many parts should we split the data into? (comes from the \emph{k}-fold cross-validation.) }
  \item{interval}{ time interval of the animation (unit in seconds) }
  \item{nmax}{ maximum number of animation frames (i.e. steps of moving the test part) }
  \item{\dots}{other arguments passed to \code{\link{savePNG}}} 
}
\details{
  The computation of sample sizes is base on \code{\link{kfcv}}. 
}
\value{
None (invisible `\code{NULL}').
}

\author{ Yihui Xie }
\note{ 
For the `leave-one-out' cross-validation, just specify \code{k} as \code{length(x)}, then the rectangles will `shrink' into single lines. 
}
\seealso{ \code{\link{kfcv}} }
\examples{
cv.ani()

# leave-one-out CV 
cv.ani(x = runif(15), k = 15) 

# save the animation in HTML pages
ani.start()
cv.ani(TRUE)
ani.stop() 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ dynamic }% __ONLY ONE__ keyword per line
\keyword{ classif }