\name{knn.ani}
\alias{knn.ani}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstrate kNN classification algorithm on the 2D plane }
\description{
  Demonstrate the process of k-Nearest Neighbour classification on the 2D plane. 
}
\usage{
knn.ani(train, test, cl, k = 10, interact = FALSE, 
    control = ani.control(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{ matrix or data frame of training set cases containing only 2 columns }
  \item{test}{ matrix or data frame of test set cases. A vector will be interpreted as a row vector for a single case. It should also contain only 2 columns. This data set will be \emph{ignored} if \code{interact = TRUE}; see \code{interact} below.}
  \item{cl}{ factor of true classifications of training set  }
  \item{k}{ number of neighbours considered.  }
  \item{interact}{logical. If \code{TRUE}, the user will have to choose a test set for himself using mouse click on the screen; otherwise compute kNN classification based on argument \code{test}.} 
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{other arguments passed to \code{\link{ani.control}}}
}
\details{
  For each row of the test set, the k nearest (in Euclidean distance) training set vectors are found, and the classification is decided by majority vote, with ties broken at random. For a single test sample point, the basic steps are: 
  \enumerate{
  \item locate the test point
  \item compute the distances between the test point and all points in the training set 
  \item find \emph{k} shortest distances and the corresponding training set points
  \item vote for the result (find the maximum in the table for the true classifications) 
  } 
  
}
\value{
A vector of class labels for the test set.
}
\references{ 
Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
}
\author{ Yihui Xie }
\note{
There is a special restriction (only two columns) on the training and test data set just for sake of the convenience for making a scatterplot. This is only a rough demonstration; for practical applications, please refer to existing kNN functions such as \code{\link[class:knn]{knn}}, etc. 

If either one of \code{train} and \code{test} is missing, there'll be random matrices prepared for them. (It's the same for \code{cl}.) 
}
\seealso{ \code{\link{ani.start}}, \code{\link{ani.stop}}, \code{\link[class]{knn}} }
\examples{
## a binary classification problem 
x = matrix(c(rnorm(80, mean = -1), rnorm(80, mean = 1)), 
    ncol = 2, byrow = TRUE) 
y = matrix(rnorm(20, mean = 0, sd = 1.2), ncol = 2) 
knn.ani(train = x, test = y, cl = rep(c("first class", "second class"),
    each = 40), k = 30, control = ani.control(interval = 2)) 

\dontrun{ 

ani.start() 
op = par(mar = c(3.5, 3.5, 3, 1), mgp = c(2, 0.5, 
    0), cex.axis = 0.75, tcl = -0.3) 
knn.ani(train = x, test = y, cl = rep(c("first class", "second class"),
    each = 40), k = 30, control = ani.control(saveANI = TRUE, 
    interval = 0.5, width = 600, height = 600)) 
par(op) 
ani.stop() 

## this is an interactive demo; just click your mouse
knn.ani(train = x, cl = rep(c("first class", "second class"), 
    each = 40), k = 30, nmax = 5, interact = TRUE) 

}
}
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ classif }
