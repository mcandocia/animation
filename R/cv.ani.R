`cv.ani` <- function(saveANI = FALSE, x = runif(150), 
    k = 10, interval = 2, nmax = 50, ...) {
    N = length(x)
    n = sample(N)
    x = x[n]
    kf = cumsum(c(1, kfcv(k, N)))
    j = 1
    for (i in 2:length(kf)) {
        if (j > nmax) 
            break
        plot(x, xlim = c(1, N), type = "n", xlab = "Sample index", 
            ylab = "Sample value", main = "Demonstration of Cross Validation", 
            bty = "l", xaxt = "n")
        xax = as.integer(pretty(1:N))
        if (xax[1] == 0) 
            xax = xax[-1]
        axis(side = 1, xax, n[xax])
        idx = kf[i - 1]:(kf[i] - 1)
        rect(kf[-length(kf)], min(x), kf[-1] - 1, max(x), border = "gray", 
            lty = 2)
        rect(kf[i - 1], min(x), kf[i] - 1, max(x), density = 10, 
            col = "green")
        points(idx, x[idx], col = "red", pch = 4, lwd = 2)
        text(mean(idx), quantile(x, prob = 0.75), "Test Set", 
            cex = 1.5, col = "red")
        points(seq(N)[-idx], x[-idx], col = "blue", pch = 1, 
            lwd = 1)
        text(mean(seq(N)[-idx]), quantile(x, prob = 0.25), "Training Set", 
            cex = 1.5, col = "blue")
        if (saveANI) 
            savePNG(n = j, ...)
        Sys.sleep(interval)
    }
    invisible(NULL)
} 
