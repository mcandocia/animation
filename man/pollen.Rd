\name{pollen}
\alias{pollen}
\alias{pollen}
\title{Synthetic dataset about the geometric features of pollen grains.}
\usage{data(pollen)}
\description{Synthetic dataset about the geometric features of pollen grains.
There are 3848 observations on 5 variables. From the 1986 ASA Data
Exposition dataset, made up by David Coleman of RCA Labs.}
\details{Format: a data frame with 3848 observations on the following 5
variables.  \describe{ \item{RIDGE}{a numeric vector} \item{NUB}{a
numeric vector} \item{CRACK}{a numeric vector} \item{WEIGHT}{a
numeric vector} \item{DENSITY}{a numeric vector} }

Source: collected from Statlib Datasets Archive:
\url{http://stat.cmu.edu/datasets/}}
\docType{data}
\keyword{datasets}
\examples{data(pollen)

## some dense points in the center?
plot(pollen[, 1:2], pch = 20, col = rgb(0, 0, 0, 0.1))

## see demo('pollen', package = 'animation') for a 3D demo;
## truth is there!}






