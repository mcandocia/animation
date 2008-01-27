`sample.cluster` <-
function(pop = ceiling(10 * runif(10, 
    0.2, 1)), size = 3, control = ani.control(interval = 0.2), 
    ...) {
    control = checkargs(control, ...) 
    if (size > length(pop)) 
        stop("sample size must be smaller than the number of clusters")
    ncol = max(pop)
    nrow = length(pop)
    op = par(mar = rep(0.1, 4), xaxs = "i", yaxs = "i")
    for (i in 1:control$nmax) {
        plot(1, axes = FALSE, ann = FALSE, type = "n", xlim = c(0.5, 
            ncol + 0.5), ylim = c(0.5, nrow + 0.5))
        rect(rep(0.5, nrow), seq(0.5, nrow, 1), rep(ncol + 0.5, 
            nrow), seq(1.5, nrow + 1, 1), col = c("bisque", "white")[rep(1:2, 
            length = nrow)])
        idx = sample(nrow, size)
        for (j in 1:nrow) {
            points(1:pop[j], rep(j, pop[j]), pch = 19, col = "blue")
            if (j %in% idx) 
                points(1:pop[j], rep(j, pop[j]), col = "red", 
                  cex = 3, lwd = 2)
        }
        if (control$saveANI) 
            savePNG(n = i, width = control$width, height = control$height)
        else Sys.sleep(control$interval)
    }
    par(op)
    invisible(NULL)
}

