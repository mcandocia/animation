`brownian.motion` <- function(n = 10, main = "Demonstration of Brownian Motion", 
    xlim = c(-20, 20), ylim = c(-20, 20), pch = 21, cex = 5, 
    col = "red", bg = "yellow", control = ani.control(nmax = 100, 
    interval = 0.05), ...) {
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
        Sys.sleep(control$interval)
        i = i + 1
    }
    invisible(NULL)
} 
