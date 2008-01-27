`brownian.motion` <- function(n = 10, main = "Demonstration of Brownian Motion", 
    xlim = c(-20, 20), ylim = c(-20, 20), pch = 21, cex = 5, 
    col = "red", bg = "yellow", control = ani.control(nmax = 100, 
    interval = 0.05), ...) {
    control = checkargs(control, ...) 
    x = rnorm(n)
    y = rnorm(n)
    i = 1
    while (i <= control$nmax) {
        plot(x, y, main = main, xlim = xlim, ylim = ylim, pch = pch, 
            cex = cex, col = col, bg = bg)
        text(x, y)
        x = x + rnorm(n)
        y = y + rnorm(n)
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
        i = i + 1
    }
    invisible(NULL)
} 
