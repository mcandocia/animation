`brownian.motion` <-
function(saveANI = FALSE, n = 10, 
    interval = 0.05, nmax = 100, main = "Demonstration of Brownian Motion", 
    xlim = c(-20, 20), ylim = c(-20, 20), pch = 21, cex = 5, 
    col = "red", bg = "yellow", ...) {
    x = rnorm(n)
    y = rnorm(n)
    i = 1
    while (i <= nmax) {
        plot(x, y, main = main, xlim = xlim, ylim = ylim, pch = pch, 
            cex = cex, col = col, bg = bg, ...)
        text(x, y)
        x = x + rnorm(n)
        y = y + rnorm(n)
        if (saveANI) 
            savePNG(n = i)
        Sys.sleep(interval)
        i = i + 1
    }
    invisible(NULL) 
}

