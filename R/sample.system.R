`sample.system` <-
function(nrow = 10, ncol = 10, size = 15, 
    control = ani.control(interval = 0.2), ...) {
    control = checkargs(control, ...) 
    n = nrow * ncol
    if (size > n) 
        stop("sample size must be smaller than the population")
    x = cbind(rep(1:ncol, nrow), gl(nrow, ncol))
    op = par(mar = rep(0.1, 4))
    for (i in 1:control$nmax) {
        plot(x, pch = 19, col = "blue", axes = FALSE, ann = FALSE)
        points(x[seq(sample(n, 1), by = n%/%size, length = size)%%n, 
            ], col = "red", cex = 3, lwd = 2)
        box()
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
    }
    par(op)
    invisible(NULL)
}

