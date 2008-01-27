`bisection.method` <-
function(FUN = function(x) x^2 - 
    4, rg = c(-1, 10), tol = 0.001, interact = FALSE, control = ani.control(), 
    ...) {
    control = checkargs(control, ...)
    if (interact) {
        curve(FUN, min(rg), max(rg), xlab = "x", ylab = eval(substitute(expression(f(x) == 
            y), list(y = body(FUN)))), main = "Locate the interval for finding root")
        rg = unlist(locator(2))[1:2]
    }
    l = min(rg)
    u = max(rg)
    if (FUN(l) * FUN(u) > 0) 
        stop("The function must have opposite signs at the lower and upper boundof the range!")
    mid = FUN((l + u)/2)
    i = 1
    bd = rg
    while (abs(mid) > tol & i <= control$nmax) {
        curve(FUN, min(rg), max(rg), xlab = names(formals(FUN)), ylab = eval(substitute(expression(f(x) == 
            y), list(y = body(FUN)))), main = eval(substitute(expression("Root-finding by Bisection Method:" ~ 
            y == 0), list(y = body(FUN)))))
        abline(h = 0, col = "gray")
        abline(v = bd, col = "red", lty = 2)
        abline(v = (l + u)/2, col = "blue")
        arrh = mean(par("usr")[3:4])
        if (u - l > 0.001 * diff(par("usr")[1:2])/par("din")[1]) 
            arrows(l, arrh, u, arrh, code = 3, col = "gray", 
                length = par("din")[1]/2^(i + 2))
        mtext(paste("Current root:", (l + u) / 2), 4) 
        bd = c(bd, (l + u)/2)
        assign(ifelse(mid * FUN(l) > 0, "l", "u"), (l + u)/2)
        mid = FUN((l + u)/2)
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
        i = i + 1
    }
    invisible(list(root = (l + u)/2, value = mid, iter = i - 
        1))
}

