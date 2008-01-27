`conf.int` <-
function(level = 0.95, size = 50, control = ani.control(interval = 0.3), 
    ...) {
    control = checkargs(control, ...)
    n = control$nmax
    d = replicate(n, rnorm(size))
    m = colMeans(d)
    z = qnorm(1 - (1 - level)/2)
    y0 = m - z/sqrt(size)
    y1 = m + z/sqrt(size)
    rg = range(c(y0, y1))
    cvr = y0 < 0 & y1 > 0
    xax = pretty(1:n)
    for (i in 1:n) {
        plot(1:n, ylim = rg, type = "n", xlab = "Samples", ylab = "Confidence interval", 
            main = expression("CI: [" ~ bar(x) - z[alpha/2] * 
                sigma/sqrt(n) ~ ", " ~ bar(x) + z[alpha/2] * 
                sigma/sqrt(n) ~ "]"), xaxt = "n")
        axis(1, xax[xax <= i])
        abline(h = 0, lty = 2)
        arrows(1:i, y0[1:i], 1:i, y1[1:i], length = par("din")[1]/n * 
            0.5, angle = 90, code = 3, col = c("red", "gray")[cvr[1:i] + 
            1])
        points(1:i, m[1:i], col = c("red", "gray")[cvr[1:i] + 
            1])
        legend("topright", legend = table(cvr[1:i]), fill = c("red", 
            "gray"), bty = "n")
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
    }
    CI = cbind(y0, y1)
    colnames(CI) = paste(round(c((1 - level)/2, 1 - (1 - level)/2), 
        2) * 100, "%")
    rownames(CI) = 1:n 
    invisible(list(level = level, size = size, CI = CI, CR = mean(cvr)))
}

