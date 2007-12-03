`boot.iid` <- function(saveANI = FALSE, x = runif(20), 
    statistic = mean, m = length(x), interval = 1, nmax = 40, 
    ...) {
    xx = statistic(sample(x, m, TRUE))
    layout(matrix(1:2, 2))
    par(mar = c(1.5, 3, 2, 0.1), cex.main = 1, cex.lab = 0.8, 
        cex.axis = 0.8, mgp = c(2, 0.5, 0), tcl = -0.3)
    for (i in 1:nmax) {
        idx = sample(length(x), m, TRUE)
        xx = c(xx, statistic(x[idx]))
        plot(x, pch = 19, col = "blue", cex = 1.5, main = "Demonstration of bootstrapping for i.i.d data", 
            xlab = "", ylab = "x")
        sunflowerplot(idx, x[idx], add = TRUE, col = "red", cex = 2)
        hist(xx, freq = FALSE, main = "Distribution of bootstrap estimates", 
            col = "bisque")
        lines(density(xx), col = "red")
        rug(xx) 
        if (saveANI) 
            savePNG(n = i, ...)
        Sys.sleep(interval)
    }
    invisible(list(t0 = statistic(x), tstar = xx))
} 
