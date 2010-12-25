\name{pageview}
\alias{pageview}
\alias{pageview}
\title{Page views from Sep 21, 2007 to Dec 2, 2007 of Yihui's website.}
\description{Page views from Sep 21, 2007 to Dec 2, 2007 of Yihui's website.}
\details{The data is collected by Awstats for the website \url{http://yihui.name}.}
\docType{data}
\keyword{datasets}
\examples{data(pageview)
plot(pageview[, 1:2], type = "b", col = "red", main = "Number of Visits in Yihui's Web")
# partial auto-correlation
pacf(pageview$visits)}

