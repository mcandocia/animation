`vi.lilac.chaser` <-
function(np = 16, col = "magenta", 
    bg = "gray", cex.p = 7, cex.c = 5, control = ani.control(interval = 0.05, 
        nmax = 30), ...) {
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
            Sys.sleep(control$interval)
        }
    }
    par(op)
    invisible(NULL)
}

