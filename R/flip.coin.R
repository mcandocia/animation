`flip.coin` <- function(faces = 2, prob = NULL, border = "white", 
    col = 1:2, type = "p", pch = 21, bg = "transparent", control = ani.control(interval = 0.2, 
        nmax = 100), ...) {
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
    if (length(faces) == 1) {
        faces = as.factor(seq(faces))
    }
    else {
        faces = as.factor(faces)
    }
    if (length(faces) < 2) 
        stop("'faces' must be at least 2")
    lv = levels(faces)
    n = length(lv)
    res = sample(faces, control$nmax, replace = TRUE, prob = prob)
    frq = table(res)/control$nmax
    ylm = max(frq)
    x = runif(control$nmax, 1.1, 1.9)
    y = runif(control$nmax, 0, ylm)
    col = rep(col, length = n)
    y0 = numeric(n)
    step = 1/control$nmax
    for (i in 1:control$nmax) {
        plot(1, xlim = c(0, 2), ylim = c(0, ylm * 1.04), type = "n", 
            axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
        abline(v = 1)
        axis(1, (1:n - 0.5)/n, lv)
        axis(2)
        box()
        mtext("Frequency", side = 2, line = 2)
        mtext("Flip 'coins'", side = 4)
        k = as.integer(res[1:i])
        points(x[1:i], y[1:i], cex = 3, col = col[k], type = type, 
            pch = pch, bg = bg)
        text(x[1:i], y[1:i], res[1:i], col = col[k])
        y0[k[i]] = y0[k[i]] + 1
        for (j in 1:n) {
            x0 = (j - 1)/n
            if (y0[j] > 0) 
                rect(x0, (1:y0[j] - 1) * step, x0 + 1/n, (1:y0[j]) * 
                  step, border = border, col = col[j])
        }
        box()
        abline(v = 1)
        axis(3, (1:n - 0.5)/n, paste(y0, " (", format(y0/control$nmax, 
            digits = 3, nsmall = 3), ")", sep = ""), tcl = 0, 
            mgp = c(0, 0.5, 0))
        axis(1, 1.5, paste("Number of Tosses:", i), tcl = 0)
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        Sys.sleep(control$interval)
    }
    invisible(as.matrix(frq)[, 1])
} 
