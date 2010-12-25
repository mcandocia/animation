\name{HuSpeech}
\alias{HuSpeech}
\alias{HuSpeech}
\title{Word Counts of a Speech by Chinese President Hu.}
\description{Word Counts of a Speech by Chinese President Hu.
This speech came on the 30th anniversary of China's economic reform in 1978.}
\details{On Dec 18, 2008, Chinese President Hu gave a speech on the 30th anniversary
of China's economic reform in 1978, and this data has recorded the number
of words used in each paragraph of his speech.}
\docType{data}
\keyword{datasets}
\examples{data(HuSpeech)
# clear pattern: 1/3 short, 1/3 long, 1/3 short again
plot(HuSpeech, type = "b", pch = 20, xlab = "paragraph index", 
    ylab = "word count")
# see ?moving.block for an animation example}

