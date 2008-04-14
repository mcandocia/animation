\name{cv.nfeaturesLDA}
\alias{cv.nfeaturesLDA}
\title{ Cross-validation to find the optimum number of features (variables) in LDA }
\description{
  For a classification problem, usually we wish to use as less variables as possible because of difficulties brought by the high dimension. This function has provided an illustration of the process of finding out the optimum number of variables using k-fold cross-validation in a linear discriminant analysis (LDA).
}
\usage{
cv.nfeaturesLDA(data = matrix(rnorm(600), 30), cl = gl(3, 10), 
    k = 5, cex.rg = c(0.5, 3), col.av = c("blue", "red"), 
    control = ani.control(nmax = 10), ...)
}
\arguments{
  \item{data}{ a data matrix containg the predictors in columns }
  \item{cl}{ a factor indicating the classification of the rows of \code{data} }
  \item{k}{ the number of folds }
  \item{cex.rg}{ the range of the magnification to be used to the points in the plot }
  \item{col.av}{ the two colors used to respectively denote rates of correct predictions in the i-th fold and the average rates for all k folds }
  \item{control}{ control parameters for the animation; see \code{\link{ani.control}} }
  \item{\dots}{ other arguments passed to \code{\link{ani.control}} }
}
\details{
  The procedure is like this:
  
Split the whole data randomly into k folds:

For the number of features g = 1, 2, ..., gmax, choose g features that have the largest discriminatory power (measured by the F-statistic in ANOVA):

For the fold i (i = 1, 2, ..., k):

Train a LDA model without the i-th fold data, and predict with the i-th fold for a proportion of correct predictions \eqn{p_{gi}}{p\_gi};

Average the 5 proportions to get the correct rate \eqn{p_g}{p\_g};

Determine the optimum number of features with the largest p.

}
\value{
A list containing
  \item{accuracy }{a matrix in which the element in the i-th row and j-th column is the rate of correct predictions based on LDA, i.e. build a LDA model with j variables and predict with data in the i-th fold (the test set) }
  \item{optimum }{the optimum number of features based on the cross-validation}
}
\references{ 
Maindonald J, Braun J (2007). \emph{Data Analysis and Graphics Using R - An Example-Based
Approach}. Cambridge University Press, 2nd edition. pp. 400
}
\author{ Yihui Xie }
\seealso{ \code{\link{kfcv}}, \code{\link{cv.ani}}, \code{\link[MASS]{lda}} }
\examples{
op = par(pch = 19, mar = c(3, 3, 0.2, 0.7))
cv.nfeaturesLDA(interval = 0.05)
par(op)
}
\keyword{ multivariate }
\keyword{ dynamic }
\keyword{ dplot }
\keyword{ classif }
