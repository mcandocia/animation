`newton.method` <-
function(FUN = function(x) x^2 - 
    4, init = 10, rg = c(-1, 10), tol = 0.001, interact = FALSE, 
    control = ani.control(interval = 2), ...) {
    control = checkargs(control, ...)
    if (interact) {
        curve(FUN, min(rg), max(rg), xlab = "x", ylab = eval(substitute(expression(f(x) == 
            y), list(y = body(FUN)))), main = "Locate the starting point")
        init = unlist(locator(1))[1]
    }
    i = 1
    nms = names(formals(FUN))
    grad = deriv(as.expression(body(FUN)), nms, function.arg = TRUE)
    x = c(init, init - FUN(init)/attr(grad(init), "gradient"))
    gap = FUN(x[2])
    while (abs(gap) > tol & i <= control$nmax & !is.na(x[i + 
        1])) {
        curve(FUN, min(rg), max(rg), xlab = nms, ylab = eval(substitute(expression(f(x) == 
            y), list(y = body(FUN)))), main = eval(substitute(expression("Root-finding by Newton-Raphson Method:" ~ 
            y == 0), list(y = body(FUN)))))
        abline(h = 0, col = "gray")
        segments(x[1:i], rep(0, i), x[1:i], FUN(x[1:i]), col = "blue")
        segments(x[1:i], FUN(x[1:i]), x[2:(i + 1)], rep(0, i), 
            col = "red")
        points(x, rep(0, i + 1), col = "red")
        points(x[i], FUN(x[i]), col = "red")
        mtext(paste("Current root:", x[i + 1]), 4)
        gap = FUN(x[i + 1])
        x = c(x, x[i + 1] - FUN(x[i + 1])/attr(grad(x[i + 1]), 
            "gradient"))
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
        i = i + 1
    }
    invisible(list(root = x[i], value = gap, iter = i - 1))
}

