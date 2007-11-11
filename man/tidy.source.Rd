\name{tidy.source}
\alias{tidy.source}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ `Tidy' up R code }
\description{
  Actually this function has nothing to do with code optimization; it just returns parsed source code. 
}
\usage{
tidy.source(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ a string: location of the source code }
}
\details{
  This function helps the user to tidy up his source code in a sense that necessary indents and spaces will be added, etc. See \code{\link[base:parse]{parse}}.
}
\value{
None (invisible `\code{NULL}'). `Clean' code will be returned to the console unless the output is redirected by `sink'. 
}

\author{ Yihui Xie }

\seealso{ \code{\link[base:parse]{parse}}, \code{\link[base:cat]{cat}} }
\examples{
tidy.source(file.path(system.file(package = "graphics"), 
  "demo", "image.R"))
}
\keyword{ IO }

