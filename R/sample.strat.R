`sample.strat` <-
function(pop = ceiling(10 * runif(10, 
    0.5, 1)), size = ceiling(pop * runif(length(pop), 0, 0.5)), 
    control = ani.control(interval = 0.2), ...) {
    extraArgs = list(...)
    if (length(extraArgs)) {
        if ("interval" %in% names(extraArgs)) 
            ani.control(...)
        controlargs = names(formals(ani.control))
        idx = match(names(extraArgs), controlargs, nomatch = 0)
        if (any(idx == 0)) 
            stop("Argument ", names(extraArgs)[idx == 0], "not matched")
        control[names(extraArgs)] = extraArgs
    }
    if (any(size > pop)) 
        stop("sample size must be smaller than population")
    ncol = max(pop)
    nrow = length(pop)
    size = rep(size, length = nrow) 
    op = par(mar = rep(0.1, 4), xaxs = "i", yaxs = "i")
    for (i in 1:control$nmax) {
        plot(1, axes = FALSE, ann = FALSE, type = "n", xlim = c(0.5, 
            ncol + 0.5), ylim = c(0.5, nrow + 0.5))
        rect(rep(0.5, nrow), seq(0.5, nrow, 1), rep(ncol + 0.5, 
            nrow), seq(1.5, nrow + 1, 1), col = c("bisque", "white")[rep(1:2, 
            length = nrow)])
        for (j in 1:nrow) {
            points(1:pop[j], rep(j, pop[j]), pch = 19, col = "blue")
            points(sample(pop[j], size[j]), rep(j, size[j]), 
                col = "red", cex = 3, lwd = 2)
        }
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        Sys.sleep(control$interval)
    }
    par(op)
    invisible(NULL)
}

