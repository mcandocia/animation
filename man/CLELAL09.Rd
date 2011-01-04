\name{CLELAL09}
\alias{CLELAL09}
\alias{CLELAL09}
\title{The NBA game between CLE Cavaliers and LAL Lakers on Dec 25, 2009.}
\usage{data(CLELAL09)}
\description{The NBA game between CLE Cavaliers and LAL Lakers on Dec 25, 2009.}
\details{Cleveland Cavaliers played against Los Angeles Lakers at Staples
Center in LA on Dec 25, 2009 and won the game by 102:87. This data
recorded the locations of players on the court and the results of
the shots.

Format:
a data frame with 455 observations on the following 6 variables.
\describe{
\item{\code{player}}{a character vector: the current player}
\item{\code{time}}{a character vector: the time}
\item{\code{period}}{a numeric vector: the period (1 - 4)}
\item{\code{realx}}{a numeric vector: the x-axis location}
\item{\code{realy}}{a numeric vector: the y-axis location}
\item{\code{result}}{a factor with levels \code{made} \code{missed}}
}

Source: \url{http://www.basketballgeek.com/data/} (transformed
based on the original data)}
\docType{data}
\note{We view the court with CLE in the left and LAL in the right:
\code{realx} is the distance to the left border of CLE's court,
and \code{realy} is the distance to the bottom border of the
court; notice that the size of the court is \eqn{94 \times 50}{94
x 50} (feet).}
\keyword{datasets}
\examples{library(animation)
data(CLELAL09)
## see demo('CLEvsLAL', package = 'animation')
##   for a `replay' of the game}

