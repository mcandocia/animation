`buffon.needle` <-
function(saveANI = FALSE, l = 0.8, d = 1, 
    interval = 0.05, nmax = 100, redraw = TRUE, ...) {
    j = 1
    n = 0
    PI = rep(NA, nmax)
    x = y = x0 = y0 = phi = ctr = NULL
    layout(matrix(c(1, 2, 4, 1, 3, 4), nrow = 3), height = c(1, 
        6, 3))
    while (j <= nmax) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Simulation of Buffon's Needle", cex = 1.2, 
            font = 2)
        par(mar = c(3, 3, 0, 1))
        plot(1, xlim = c(-0.5 * l, 1.5 * l), ylim = c(0, 2 * 
            d), type = "n", xlab = "", ylab = "", axes = FALSE)
        axis(1, c(0, l), c("", ""), tcl = -1)
        axis(1, 0.5 * l, "L", font = 3, tcl = 0, cex.axis = 1.5, 
            mgp = c(0, 0.5, 0))
        axis(2, c(0.5, 1.5) * d, c("", ""), tcl = -1)
        axis(2, d, "D", font = 3, tcl = 0, cex.axis = 1.5, mgp = c(0, 
            0.5, 0))
        box()
        bd = par("usr")
        rect(bd[1], 0.5 * d, bd[2], 1.5 * d, col = "lightgray")
        abline(h = c(0.5 * d, 1.5 * d), lwd = 2)
        phi = c(phi, runif(1, 0, pi))
        ctr = c(ctr, runif(1, 0, 0.5 * d))
        y = c(y, sample(c(0.5 * d + ctr[j], 1.5 * d - ctr[j]), 
            1))
        x = c(x, runif(1, 0, l))
        x0 = c(x0, 0.5 * l * cos(phi[j]))
        y0 = c(y0, 0.5 * l * sin(phi[j]))
        if (redraw) {
            segments(x - x0, y - y0, x + x0, y + y0, col = "red")
        }
        else {
            segments(x[j] - x0[j], y[j] - y0[j], x[j] + x0[j], 
                y[j] + y0[j], col = "red")
        }
        xx = seq(0, pi, length = 200)
        plot(xx, 0.5 * l * sin(xx), type = "l", ylim = c(0, 0.5 * 
            d), bty = "l", xlab = "", ylab = "", col = "gray")
        if (redraw) {
            points(phi, ctr, pch = 20, col = c("red", "blue")[as.numeric(ctr > 
                0.5 * l * sin(phi)) + 1])
        }
        else {
            points(phi[j], ctr[j], pch = 20, col = c("red", "blue")[as.numeric(ctr[j] > 
                0.5 * l * sin(phi[j])) + 1])
        }
        text(pi/2, 0.4 * l, expression(y == frac(L, 2) * sin(phi)), 
            cex = 1.5)
        n = n + (ctr[j] <= 0.5 * l * sin(phi[j]))
        if (n > 0) 
            PI[j] = 2 * l * j/(d * n)
        par(mar = c(3, 3, 0, 1))
        plot(PI, ylim = c(0.6 * pi, 1.4 * pi), xlab = "Dropping times", 
            ylab = "", type = "l", mgp = c(2, 0.5, 0))
        abline(h = pi, lty = 2, col = "red")
        legend("topright", legend = c(expression(pi), expression(hat(pi))), 
            lty = 2:1, col = c("red", "black"), bty = "n", cex = 1.2)
        if (saveANI) 
            savePNG(n = j, ...)
        j = j + 1
        Sys.sleep(interval)
    }
    invisible(PI)
}

