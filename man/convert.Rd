\name{convert}
\alias{im.convert}
\title{A wrapper for the `convert' utility of ImageMagick or GraphicsMagick.}
\usage{im.convert(files, interval = ani.options("interval"), 
    loop = 0, output = "animation.gif", extra.opts = "", outdir = getwd(), 
    convert = c("convert", "gm convert"), cmd.fun, clean = FALSE)
gm.convert(..., convert = "gm convert")
}
\description{A wrapper for the `convert' utility of ImageMagick or GraphicsMagick.}
\details{\code{im.convert}: The function \code{im.convert} simply wraps the arguments of the
\command{convert} utility of ImageMagick to make it easier to call
ImageMagick in R; similarly, the function \code{gm.convert} is a wrapper for
the command \command{gm convert} of GraphicsMagick. These two functions are
nearly identical -- the only difference is the default value for the \code{convert}
argument (\code{'convert'} for \code{im.convert}, and \code{'gm convert'} for
\code{gm.convert}). The main purpose of this function is to create GIF animations.

}
\alias{im.convert}
\alias{gm.convert}
\value{\code{im.convert}: The path of the output if the command was successfully executed;
otherwise a failure message.

If \code{ani.options('autobrowse') == TRUE}, this function will also try to
open the output automatically.

}
\note{If \code{files} is a character vector, please make sure the order of
filenames is correct! The first animation frame will be \code{files[1]},
the second frame will be \code{files[2]}, ...

Most Windows users do not have read the boring notes below after they have
installed ImageMagick or GraphicsMagick. For the rest:

ImageMagick users -- please install ImageMagick from
\url{http://www.imagemagick.org}, and make sure the the path to \command{convert.exe}
is in your \code{'PATH'} variable, in which case the command
\command{convert} can be called without the full path.
Windows users are often very confused
about the ImageMagick and \code{'PATH'} setting, so I'll try to search
for ImageMagick in the Registry Hive by
\code{readRegistry('SOFTWARE\ImageMagick\Current')$BinPath}, thus you
might not really need to modify your \code{'PATH'} variable.

For Windows users who have installed LyX, I will also try to find the
\command{convert} utility in the LyX installation directory, so they do
not really have to install ImageMagick if LyX exists in their system
(of course, the LyX should be installed with ImageMagick).

GraphicsMagick users -- during the installation of GraphicsMagick, you will
be asked if you allow it to change the PATH variable; please do check the option.

A reported problem is \code{cmd.fun = shell} might not work under
Windows but \code{cmd.fun = system} works fine. Try this option in case of failures.}
\author{Yihui Xie <\url{http://yihui.name}>}
\references{ImageMagick: \url{http://www.imagemagick.org/script/convert.php}

GraphicsMagick: \url{http://www.graphicsmagick.org}}
\arguments{\item{files}{either a character vector of file names, or a single string
containing wildcards (e.g. \file{Rplot*.png})}
\item{interval}{time to pause between image frames in seconds (can be
a vector, which specifies the time intervals between successive frames)}
\item{loop}{iterations of the movie; set iterations to 0 to play the
animation for an infinite number of times, otherwise the animation
repeats itself up to \code{loop} times (N.B. for GIF only!)}
\item{output}{the file name of the output (with proper extensions, e.g.
gif)}
\item{extra.opts}{additional options to be passed to \command{convert}}
\item{outdir}{the output directory}
\item{convert}{the \command{convert} command; see Details}
\item{cmd.fun}{a function to invoke the OS command; by default,
\code{shell} under Windows and \code{\link[base]{system}} under other OS}
\item{clean}{logical: delete the input \code{files} or not}
\item{...}{arguments to be passed to \code{\link{im.convert}}}
}
\examples{\dontrun{
    png(file.path(tempdir(), "bm\%03d.png"))
    ani.options(interval = 0, nmax = 50)
    brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow", 
        main = "Demonstration of Brownian Motion")
    dev.off()
    
    ## filenames with a wildcard *
    bm.files = paste(tempdir(), "bm*.png", sep = .Platform$file.sep)
    im.convert(files = bm.files, interval = 0.05, output = "bm-animation1.gif")
    ## use GraphicsMagick
    gm.convert(files = bm.files, interval = 0.05, output = "bm-animation2.gif")
    
    ## or a filename vector
    bm.files = file.path(tempdir(), sprintf("bm\%03d.png", 1:50))
    im.convert(files = bm.files, interval = 0.05, output = "bm-animation3.gif")
}}
\alias{gm.convert}

