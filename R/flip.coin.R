`flip.coin` <-
function(saveANI = FALSE, faces = 2, prob = NULL, 
    interval = 0.2, nmax = 100, border = "white", ...) {
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
    res = sample(faces, nmax, replace = TRUE, prob = prob)
    frq = table(res)/nmax
    ylm = max(frq)
    plot(1, xlim = c(0, 2), ylim = c(0, ylm * 1.04), type = "n", 
        axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    abline(v = 1)
    axis(1, (1:n - 0.5)/n, lv)
    axis(2)
    box()
    mtext("Probability", side = 2, line = 2)
    mtext("Flip 'coins'", side = 4)
    x = runif(nmax, 1.1, 1.9)
    y = runif(nmax, 0, ylm)
    cl = rainbow(n)
    y0 = numeric(n)
    for (i in 1:nmax) {
        k = as.integer(res[i])
        points(x[i], y[i], cex = 3, col = cl[k], ...)
        text(x[i], y[i], res[i], col = cl[k])
        x0 = (k - 1)/length(lv)
        rect(x0, y0[k], x0 + 1/length(lv), y0[k] + 1/nmax, border = border, 
            col = cl[k])
        y0[k] = y0[k] + 1/nmax
        box()
        abline(v = 1)
        if (saveANI) 
            savePNG(n = i)
        Sys.sleep(interval)
    }
    axis(3, (1:n - 0.5)/n, round(frq, 2), tcl = 0, 
        mgp = c(0, 0.5, 0))
    return(as.matrix(frq)[, 1]) 
}

