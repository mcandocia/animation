`clt.ani` <-
function(n = 30, FUN = runif, control = ani.control(interval = 0.1), 
    ...) {
    control = checkargs(control, ...) 
    op = par(mar = c(3, 3, 2, 0.5), mgp = c(1.5, 0.5, 0), tcl = -0.3)
    layout(matrix(c(1, 3, 2, 3), 2))
    x = FUN(n)
    xbar = mean(x)
    for (i in 1:control$nmax) {
        x = FUN(n)
        xbar = c(xbar, mean(x))
        plot(ecdf(x), main = expression("ECDF of" ~ ~italic(x)))
        hist(x, freq = FALSE, main = expression("Density Estimation of" ~ 
            ~italic(x)), col = "bisque")
        lines(density(x), col = "red")
        hist(xbar, freq = FALSE, main = expression("Density Estimation of" ~ 
            ~italic(bar(x))), xlab = expression(italic(bar(x))), 
            col = "bisque")
        lines(density(xbar), col = "red")
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
    }
    par(op)
    invisible(NULL)
}

