`mwar.ani` <-
function(x, k = 15, conf = 2, control = ani.control(), 
    ...) {
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
    if(missing(x))
        x = sin(seq(0, 2 * pi, length = 50)) + rnorm(50, sd = 0.2) 
    n = length(x)
    if (k > n) 
        stop("The window width k must be smaller than the length of x!")
    idx = matrix(1:k, nrow = k, ncol = n - k + 1) + matrix(rep(0:(n - 
        k), each = k), nrow = k, ncol = n - k + 1)
    phi = se = numeric(ncol(idx))
    j = 1
    for (i in 1:ncol(idx)) {
        if (j > control$nmax) 
            break
        fit = arima(x[idx[, i]], order = c(1, 0, 0))
        phi[i] = coef(fit)["ar1"]
        se[i] = sqrt(vcov(fit)[1, 1])
        j = j + 1
    }
    op = par(mfrow = c(2, 1), mgp = c(2, 0.5, 0), cex.axis = 0.8, 
        cex.lab = 0.85, tcl = -0.3)
    U = phi + conf * se
    L = phi - conf * se
    j = 1
    minx=maxx=NULL 
    for (i in 1:ncol(idx)) {
        if (j > control$nmax) 
            break
        par(mar = c(1.5, 3, 0.1, 0.1))
        plot(x, xlab = "", ylab = "Original data")
        minx =c(minx,min(x[idx[, i]]))
        maxx =c(maxx,max(x[idx[, i]]))
        rect(1:i, minx, k:(i + k - 1), maxx, 
            lty = 2, border = 1:i)
        par(mar = c(1.5, 3, 0.5, 0.1))
        plot(x,xlim=c(1,n), ylim=range(c(U, 
            L)), type = "n", ylab = "AR(1) coefficient")
        arrows(1:i + k/2 - 0.5, L[1:i], 1:i + k/2 - 0.5, U[1:i], angle = 90, 
            code = 3, length = 0.05, col = 1:i)
        points(1:i + k/2 - 0.5, phi[1:i], pch = 21, col = "red", 
            bg = "yellow",type='o')
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        Sys.sleep(control$interval)
        j = j + 1
    }
    par(op)
    invisible(list(phi = phi, lower = L, upper = U)) 
}

