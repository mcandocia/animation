`kmeans.ani` <- function(x = matrix(runif(100), ncol = 2), 
    centers = 2, control = ani.control(interval = 2, nmax = 30), 
    ...) {
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
    x = as.matrix(x)
    if (ncol(x) != 2) 
        stop("'x' must contain ONLY 2 columns!")
    ocluster = sample(centers, nrow(x), replace = TRUE)
    centers = x[sample(nrow(x), centers), ]
    dst = matrix(nrow = nrow(x), ncol = nrow(centers))
    j = jj = 1
    while (jj <= control$nmax) {
        plot(x, pch = ocluster, col = ocluster, main = "Move Cluster Centers")
        points(centers, pch = 1:nrow(centers), cex = 3, lwd = 2, 
            col = 1:nrow(centers))
        Sys.sleep(control$interval)
        if (control$saveANI) 
            savePNG(n = j, width = control$width, height = control$height)
        j = j + 1
        for (i in 1:nrow(centers)) {
            dst[, i] = sqrt(apply((t(t(x) - unlist(centers[i, 
                ])))^2, 1, sum))
        }
        ncluster = apply(dst, 1, which.min)
        plot(x, type = "n", main = "Find Cluster Membership")
        points(centers, cex = 3, col = 1:nrow(centers), pch = 1:nrow(centers), 
            lwd = 2)
        for (i in 1:nrow(centers)) {
            xx = subset(x, ncluster == i)
            polygon(xx[chull(xx), ], density = 10, col = i, lty = 2)
            points(xx, pch = i, col = i)
            centers[i, ] = apply(xx, 2, mean)
        }
        if (control$saveANI) 
            savePNG(n = j, width = control$width, height = control$height)
        j = j + 1
        Sys.sleep(control$interval)
        jj = jj + 1
        if (all(ncluster == ocluster)) 
            break
        ocluster = ncluster
    }
    invisible(list(cluster = ncluster, centers = centers))
} 
