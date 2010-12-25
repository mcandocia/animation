\name{ani.stop}
\alias{ani.stop}
\title{Write the HTML animation page.}
\description{Write the HTML animation page.
And restore previous options such as \code{prompt}; then close the graphical
device opened in \code{\link{ani.start}}.}
\value{None (invisible `\code{NULL}'); a string will be printed in the
console indicating where is the HTML file.}
\note{The content of the HTML file completely depends on the parameters set
in \code{\link{ani.options}}.}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{ani.options}}, \code{\link{ani.start}}}
\references{\url{http://animation.yihui.name/animation:create_html_animation_page}}
\keyword{utilities}
\examples{\dontrun{
    
    # save the animation in HTML pages and auto-browse it
    ani.options(ani.width = 600, ani.height = 500, interval = 0.2)
    ani.start()
    boot.iid()
    ani.stop()
    
}}

