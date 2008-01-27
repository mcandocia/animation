\name{flip.coin}
\alias{flip.coin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Probability in flipping coins }
\description{
  In the first class of learning probability theory, we usually begin with flipping coins or tossing dice. This function provides a simulation to such a process and computes the frequency for `heads' and `tails'. 
}
\usage{
flip.coin(faces = 2, prob = NULL, border = "white", col = 1:2, 
    type = "p", pch = 21, bg = "transparent", 
    control = ani.control(interval = 0.2, nmax = 100), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faces}{ an integer or a character vector. See details below. }
  \item{prob}{ the probability vector of showing each face. If \code{NULL}, each face will be shown in the same probability. }
  \item{border}{ The border style for the rectangles which stand for probabilities. }
  \item{col}{ The colors to annotate different faces of the `coin'. } 
  \item{type, pch, bg}{ See \code{\link[graphics:points]{points}}.} 
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}}}
}
\details{
  If \code{faces} is a single integer, say 2, a sequence of integers from 1 to \code{faces} will be used to denote the faces of a coin; otherwise this character vector just gives the names of each face.
  
  When the \emph{i}-th face shows up, a colored thin rectangle will be added to the corresponding place (the \emph{i}-th bar), and there will be corresponding annotations for the number of tosses and frequencies. 
  
  The special argument \code{border} is for consideration of a too large number of flipping, in which case if you still draw borders of rectangles, the graph will almost only show the color of the borders, thus we should specify it as \code{NA}. 
  
  At last the frequency for each face will be computed and shown in the header of the plot -- this shall be close to \code{prob} if \code{nmax} is large enough. 
}
\note{
  You may change the colors of each face using the argument \code{col} (repeated if shorter than the number of faces). 
} 
\value{
A vector of frequencies (simulated probabilities). 
}

\author{ Yihui Xie }
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}} }
\examples{
# a coin would stand on the table?? just kidding :) 
flip.coin(faces = c("Head", "Stand", "Tail"), interval = 0.2, 
    nmax = 50, type = "n", prob = c(0.45, 0.1, 0.45), col =c(1, 2, 4)) 

\dontrun{
 
# HTML animation page
ani.start()
flip.coin(saveANI = TRUE, interval = 0.1, nmax = 30, bg = "yellow",
    width = 600, height = 500) 
ani.stop() 

}
}

\keyword{ hplot }
\keyword{ dplot }
\keyword{ distribution }
\keyword{ dynamic }
