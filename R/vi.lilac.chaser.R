`vi.lilac.chaser` <-
function(np = 16, col = "magenta", 
    bg = "gray", cex.p = 7, cex.c = 5, control = ani.control(interval = 0.05, 
        nmax = 30), ...) {
    control = checkargs(control, ...)
    op = par(bg = bg, xpd = NA)
    x = seq(0, 2 * pi, length = np)
    for (j in 1:control$nmax) {
        for (i in 1:np) {
            plot.new() 
            plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
            points(sin(x[-i]), cos(x[-i]), col = col, cex = cex.p, 
                pch = 19)
            points(0, 0, pch = "+", cex = cex.c, lwd = 2)
            if (control$saveANI) 
                savePNG(n = i, width = control$width, height = control$height)
            else Sys.sleep(control$interval)
        }
    }
    par(op)
    invisible(NULL)
}

