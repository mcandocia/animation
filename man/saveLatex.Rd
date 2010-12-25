\name{saveLatex}
\alias{saveLatex}
\title{Insert animations into a LaTeX document and compile it.}
\usage{saveLatex(expr, interval = 1, nmax, ani.dev = "pdf", 
    outdir = ".", ani.basename = "Rplot", ani.ext = "pdf", num = ifelse(ani.ext == 
        "pdf", "", "\%d"), ani.first, ani.opts, centering = TRUE, 
    caption, label, pkg.opts, documentclass = "article", latex.filename = "animation.tex", 
    pdflatex = "pdflatex", install.animate = TRUE, overwrite = TRUE, 
    ...)
}
\description{Insert animations into a LaTeX document and compile it.
Record animation frames and insert them into a LaTeX document with the
\code{animate} package. Compile the document if an appropriate LaTeX
command is provided.}
\details{This is actually a wrapper to generate a LaTeX document using R. The
document uses the LaTeX package called \code{animate} to insert animations
into PDF's. When we pass an R expression to this function, the expression
will be evaluated and recorded by a grahpics device (typically
\code{\link[grDevices]{png}} and \code{\link[grDevices]{pdf}}). At last, a
LaTeX document will be created and compiled if an appropriate LaTeX command
is provided. And the final PDF output will be opened with the PDF viewer
set in \code{getOption("pdfviewer")} if \code{ani.options("autobrowse") ==
TRUE}.}
\value{Invisible \code{NULL}}
\note{This function will detect if it was called in a Sweave environment --
if so, \code{ani.basename} will be automatically adjusted to
\code{prefix.string-label}, and the LaTeX output will not be a complete
document, but rather a single line like
\verb{\\animategraphics[ani.opts]{1/interval}{ani.basename}{}{}}

This automatic feature can be useful to Sweave users (but remember to
set the Sweave option \code{results=tex}).

When using \code{ani.dev = "png"} or other bitmap graphics devices,
all the images can be recorded only if a proper \code{num} is provided;
typically it must be \code{"\%d"}.

PDF devices are recommended because of their high quality and usually they
are more friendly to LaTeX. But sometimes the size of PDF files is much
larger.

So far animations created by the LaTeX package \pkg{animate} can only be
viewed with Acrobat Reader (Windows) or \command{acroread} (Linux).
Other PDF viewers may not support JavaScript (in fact the PDF animation is
driven by JavaScript). Linux users may need to install \command{acroread}
and set \code{options(pdfviewer = 'acroread')}.}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{saveMovie}} to convert image frames to a single
GIF/MPEG file; \code{\link{saveSWF}} to convert images to Flash;
\code{\link{ani.start}} and \code{\link{ani.stop}} to create an HTML page
containing the animation}
\references{To know more about the \code{animate} package, please refer to
\url{http://www.ctan.org/tex-archive/macros/latex/contrib/animate/}.
There are a lot of options can be set in \code{ani.opts} and
\code{pkg.opts}.}
\keyword{dynamic}
\keyword{device}
\keyword{utilities}
\arguments{\item{expr}{an expression to generate animations; use either the animation
functions (e.g. \code{brownian.motion()}) in this package or a custom
expression (e.g. \code{for(i in 1:10) plot(runif(10), ylim = 0:1)}).}
\item{interval}{duration between animation frames (unit in seconds)}
\item{nmax}{maximum number of animatio frames (if missing and the graphics
device is a bitmap device, this number will be automatically calculated);
note that we do not have to specify \code{nmax} when using PDF devices.}
\item{outdir}{the directory for output (default to be the current working
directory)}
\item{ani.dev}{the graphics device to be used to record image frames}
\item{ani.basename,ani.ext}{basename and extension of file names of
animation frames; see the Note section for a possible adjustment on
\code{ani.basename}}
\item{num}{the format for page numbers}
\item{ani.first}{an expression to be evaluated before plotting (this will
be useful to set graphical parameters in advance, e.g. \code{ani.first =
par(pch = 20)})}
\item{ani.opts}{options to control the behavior of the animation (passed to
the LaTeX macro \code{"\animategraphics"}; default to be
\code{"controls,width=\\linewidth"})}
\item{centering}{logical: whether to center the graph using the LaTeX
environment \verb{\\begin{center}} and \verb{\\end{center}}}
\item{caption,label}{caption and label for the graphics in the figure
environment}
\item{pkg.opts}{global options for the \code{animate} package}
\item{documentclass}{LaTeX document class; if \code{NULL}, the output
will not be a complete LaTeX document (only the code to generate the
PDF animation will be printed in the console)}
\item{latex.filename}{file name of the LaTeX document; if an empty string
\code{""}, the LaTeX code will be printed in the console and hence not
compiled}
\item{pdflatex}{the command for pdfLaTeX (set to \code{NULL} to ignore the
compiling)}
\item{install.animate}{copy the LaTeX style files \file{animate.sty} and
\file{animfp.sty} to \code{outdir}? If you have not installed the LaTeX
package \code{animate}, it suffices just to copy these to files.}
\item{overwrite}{whether to overwrite the existing image frames}
\item{\dots}{other arguments passed to the graphics device \code{ani.dev},
e.g. height and width}
}
\examples{\dontrun{
    
    oopt = ani.options(interval = 0.1, nmax = 100)
    ## brownian motion: note the 'loop' option in ani.opts
    ##     and how to set graphics parameters with 'ani.first'
    saveLatex({
        brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow", 
            main = "Demonstration of Brownian Motion")
    }, ani.basename = "BM", ani.opts = "controls,loop,width=0.8\\\\textwidth", 
        ani.first = par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 
            0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1), 
        latex.filename = "brownian.motion.tex")
    ani.options(oopt)
}}

