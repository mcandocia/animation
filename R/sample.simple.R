`sample.simple` <-
function(nrow = 10, ncol = 10, size = 15, 
    control = ani.control(interval = 0.2), ...) {
    extraArgs = list(...)
    if (length(extraArgs)) {
        controlargs = names(formals(ani.control))
        idx = match(names(extraArgs), controlargs, nomatch = 0)
        if (any(idx == 0)) 
            stop("Argument ", names(extraArgs)[idx == 0], "not matched")
        control[names(extraArgs)] = extraArgs
        if ("interval" %in% names(extraArgs)) 
            ani.control(...)
    }
    if (size > nrow*ncol) 
        stop("sample size must be smaller than the population")
    x = cbind(rep(1:ncol, nrow), gl(nrow, ncol))
    op = par(mar = rep(0.1, 4))
    for (i in 1:control$nmax) {
        plot(x, pch = 19, col = "blue", axes = FALSE, ann = FALSE)
        points(x[sample(nrow * ncol, size), ], col = "red", cex = 3, 
            lwd = 2)
        box()
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        Sys.sleep(control$interval)
    }
    par(op)
    invisible(NULL)
}

