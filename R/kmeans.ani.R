`kmeans.ani` <-
function(saveANI = FALSE, x, centers = 2, 
    interval = 2, nmax = 30) {
    x = as.matrix(x)
    if (ncol(x) != 2) 
        stop("'x' must contain ONLY 2 columns!")
    ocluster = sample(centers, nrow(x), replace = TRUE)
    centers = x[sample(nrow(x), centers), ]
    dst = matrix(nrow = nrow(x), ncol = nrow(centers))
    j = jj = 1
    while (jj <= nmax) {
        plot(x, pch = ocluster, col = ocluster, main = "Move Cluster Centers")
        points(centers, pch = 1:nrow(centers), cex = 3, lwd = 2, 
            col = 1:nrow(centers))
        Sys.sleep(interval)
        if (saveANI) 
            savePNG(n = j)
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
        if (saveANI) 
            savePNG(n = j)
        j = j + 1
        Sys.sleep(interval)
        jj = jj + 1
        if (all(ncluster == ocluster)) 
            break
        ocluster = ncluster
    }
    return(list(cluster = ncluster, centers = centers))
}

