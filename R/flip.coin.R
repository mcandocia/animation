`flip.coin` <- function(faces = 2, prob = NULL, border = "white", 
    col = 1:2, type = "p", pch = 21, bg = "transparent", 
    control = ani.control(interval = 0.2, nmax = 100), ...) {
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
    plot(1, xlim = c(0, 2), ylim = c(0, ylm * 1.04), type = "n", 
        axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    abline(v = 1)
    axis(1, (1:n - 0.5)/n, lv)
    axis(2)
    box()
    mtext("Frequency", side = 2, line = 2)
    mtext("Flip 'coins'", side = 4)
    x = runif(control$nmax, 1.1, 1.9)
    y = runif(control$nmax, 0, ylm)
    col = rep(col, length = n)
    y0 = numeric(n)
    for (i in 1:control$nmax) {
        k = as.integer(res[i])
        points(x[i], y[i], cex = 3, col = col[k], type = type, 
            pch = pch, bg = bg)
        text(x[i], y[i], res[i], col = col[k])
        x0 = (k - 1)/length(lv)
        rect(x0, y0[k], x0 + 1/length(lv), y0[k] + 1/control$nmax, border = border, 
            col = col[k])
        y0[k] = y0[k] + 1/control$nmax
        box()
        abline(v = 1)
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        Sys.sleep(control$interval)
    }
    axis(3, (1:n - 0.5)/n, round(frq, 2), tcl = 0, mgp = c(0, 
        0.5, 0))
    invisible(as.matrix(frq)[, 1])
} 
