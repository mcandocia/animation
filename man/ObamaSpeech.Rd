\name{ObamaSpeech}
\alias{ObamaSpeech}
\alias{ObamaSpeech}
\title{Word counts of a speech by the US President Obama.}
\usage{data(ObamaSpeech)}
\description{Word counts of a speech by the US President Obama.
This data recorded the number of words in each paragraph of Barack
Obama's speech in Chicago after winning the presidential election.}
\details{The format is: int [1:59] 2 45 52 53 11 48 28 15 50 29 ...

Source: The full text of speech is at
\url{http://www.baltimoresun.com/news/nation-world/bal-text1105,0,5055673,full.story}}
\docType{data}
\keyword{datasets}
\examples{data(ObamaSpeech)
## pattern: longer paragraph and shorter paragraph
plot(ObamaSpeech, type = "b", pch = 20, xlab = "paragraph index", 
    ylab = "word count")}



