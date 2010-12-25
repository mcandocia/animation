\name{Rosling.bubbles}
\alias{Rosling.bubbles}
\title{The bubbles animation in Hans Rosling's Talk ``Debunking third-world myths...}
\usage{Rosling.bubbles(x, y, circles, squares, rectangles, 
    stars, thermometers, boxplots, inches = TRUE, fg = par("col"), 
    bg, xlab = "x", ylab = "y", main, xlim = range(x), ylim = range(y), 
    ..., grid = TRUE, text = 1:ani.options("nmax"), text.col = rgb(0, 
        0, 0, 0.5), text.cex = 5)
}
\description{The bubbles animation in Hans Rosling's Talk ``Debunking third-world myths
with the best stats you've ever seen''.}
\details{In Hans Rosling's attractive talk ``Debunking third-world myths with the
best stats you've ever seen'', he used a lot of bubble plots to illustrate
trends behind the data over time. This function gives an imitation of those
moving bubbles, besides, as this function is based on
\code{\link[graphics]{symbols}}, we can also make use of other symbols such
as squares, rectangles, thermometers, etc.

Suppose we have observations of \eqn{n}{n} individuals over
\code{ani.options("nmax")} years. In this animation, the data of each year
will be shown in the bubbles (symbols) plot; as time goes on, certain
trends will be revealed (like those in Rosling's talk). Please note that
the arrangement of the data for bubbles (symbols) should be a matrix like
\eqn{A_{ijk}} in which \eqn{i} is the individual id (from 1 to n),
\eqn{j} denotes the \eqn{j}-th variable (from 1 to p) and \eqn{k} indicates
the time from 1 to \code{ani.options('nmax')}.

And the length of \code{x} and \code{y} should be equal to the number of
rows of this matrix.}
\value{\code{NULL}.}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link[graphics]{symbols}}}
\references{\url{http://animation.yihui.name/da:ts:hans_rosling_s_talk}

\url{http://www.ted.com/index.php/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen.html}}
\keyword{dynamic}
\arguments{\item{x,y}{the x and y co-ordinates for the centres of the bubbles
(symbols). Default to be 10 uniform random numbers in [0, 1] for each
single image frame (so the length should be 10 *
\code{ani.options("nmax")})}
\item{circles,squares,rectangles,stars,thermometers,boxplots}{different symbols; see \code{\link[graphics]{symbols}}. Default to be
\code{circles}.}
\item{inches,fg,bg,xlab,ylab,main,xlim,ylim,\dots}{see
\code{\link[graphics]{symbols}}. Note that \code{bg} has default
values taking semi-transparent colors.}
\item{grid}{logical; add a grid to the plot?}
\item{text}{a character vector to be added to the plot one by one (e.g. the
year in Rosling's talk)}
\item{text.col,text.cex}{color and magnification of the background text}
}
\examples{opar = par(mar = c(4, 4, 0.2, 0.2))
oopt = ani.options(interval = 0.1, nmax = 50)
# use default arguments (random numbers); you may try to find the real data
Rosling.bubbles()

# rectangles
Rosling.bubbles(rectangles = matrix(abs(rnorm(50 * 
    10 * 2)), ncol = 2))

\dontrun{
    # save the animation in HTML pages
    ani.options(ani.height = 450, ani.width = 600, title = "The Bubbles Animation in Hans Rosling's Talk", 
        description = "An imitation of Hans Rosling's moving bubbles.")
    ani.start()
    par(mar = c(4, 4, 0.2, 0.2))
    # with 'years' as the background
    Rosling.bubbles(text = 1951:2000)
    ani.stop()
}

ani.options(oopt)
par(opar)}

