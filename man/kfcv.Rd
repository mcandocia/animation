\name{kfcv}
\alias{kfcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sample sizes for k-fold cross-validation }
\description{
  Compute sample sizes for \emph{k}-fold cross-validation. 
}
\usage{
kfcv(k, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{ number of groups. }
  \item{N}{ total sample size. }
}
\details{
  If N/k is an integer, the sample sizes are k `N/k's (N/k, N/k, ...), otherwise the remainder will be added to the \emph{first} group. 
}
\value{
  A vector of length \code{k} containing k sample sizes. 
}

\author{ Yihui Xie }
\seealso{ \code{\link{cv.ani}} }
\examples{
kfcv(10,71) 
}
\keyword{ arith }
\keyword{ classif }% __ONLY ONE__ keyword per line
