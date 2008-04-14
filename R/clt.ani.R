`clt.ani` <-
function(obs = 100, FUN = runif, control = ani.control(interval = 0.1), 
    ...) {
    control = checkargs(control, ...) 
    op = par(mar = c(3.2, 3, 2, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3)
    for (i in 1:control$nmax) {
        xbar = apply(matrix(replicate(obs, FUN(i)), i), 2, mean)
        hist(xbar, freq = FALSE, main = substitute("Density Estimation of" ~ 
            ~italic(bar(X)[i]),list(i =i)), xlab = substitute(italic(bar(x)[i]),list(i=i)), 
            col = "bisque")
        lines(density(xbar), col = "red")
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
    }
    par(op)
    invisible(NULL)
}

