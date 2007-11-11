\name{flip.coin}
\alias{flip.coin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Probability in flipping coins }
\description{
  In the first class of learning probability theory, we usually begin with flipping coins or tossing dice. This function provides a simulation to such a process and computes the frequency for `heads' and `tails'. 
}
\usage{
flip.coin(saveANI = FALSE, faces = 2, prob = NULL, interval = 0.2, 
  nmax = 100, border = "white", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saveANI}{ logical. Should the animation frames be saved as png files? }
  \item{faces}{ an integer or a character vector. See details below. }
  \item{prob}{ the probability vector of showing each face. If \code{NULL}, each face will be shown in the same probability. }
  \item{interval}{ Time interval of the animation (unit in seconds). }
  \item{nmax}{ The maximum number of flipping. }
  \item{border}{ The border style for the rectangles which stand for probabilities. }
  \item{\dots}{ Other arguments passed to \code{\link[graphics:points]{points}}. }
}
\details{
  If \code{faces} is a single integer, for example 2, a sequence of integers from 1 to \code{faces} will be used to denote the faces of a coin; otherwise this character vector just gives the names of each face.
  
  When the \emph{i}-th face shows up, a colored thin rectangle will be added to the corresponding place (the \emph{i}-th bar). 
  
  The special argument \code{border} is for consideration of a too large number of flipping, in which case if you still draw borders of rectangles, the graph will almost only show the color of the borders, thus we should specify it as \code{NA}. 
  
  At last the frequency for each face will be computed and shown in the header of the plot -- this shall be close to \code{prob} if \code{nmax} is large enough. 
}
\value{
A vector of frequencies (simulated probabilities). 
}

\author{ Yihui Xie }
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
# a coin would stand on the table?? :) 
flip.coin(faces = c("Head", "Stand", "Tail"), interval = 0.2, 
    nmax = 50, type = "n", prob = c(0.45, 0.1, 0.45)) 

# HTML animation page
ani.start()
flip.coin(TRUE, interval = 0, nmax = 30, pch = 21, bg = "yellow") 
ani.stop() 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ dplot }
\keyword{ distribution }
\keyword{ dynamic }
