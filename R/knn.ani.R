`knn.ani` <-
function(saveANI = FALSE, train, test, cl, 
    k = 1, interval = 1, nmax = 100, interact = FALSE) {
    train <- as.matrix(train)
    if (interact) {
        plot(train, main = "Choose test set points", pch = unclass(as.factor(cl)), 
            col = "blue")
        lct = locator(n = nmax, type = "p", pch = "?", col = "red")
        test = cbind(lct$x, lct$y)
    }
    if (is.null(dim(test))) 
        dim(test) <- c(1, length(test))
    test <- as.matrix(test)
    if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    if (ncol(test) != 2 | ncol(train) != 2) 
        stop("both column numbers of 'train' and 'test' must be 2!")
    ntr <- nrow(train)
    if (length(cl) != ntr) 
        stop("'train' and 'class' have different lengths")
    if (ntr < k) {
        warning(gettextf("k = %d exceeds number %d of patterns", 
            k, ntr), domain = NA)
        k <- ntr
    }
    if (k < 1) 
        stop(gettextf("k = %d must be at least 1", k), domain = NA)
    nte <- nrow(test)
    clf = as.factor(cl)
    res = NULL
    j = 1
    jj = 1
    for (i in 1:nrow(test)) {
        if (jj > nmax) 
            break
        idx = rank(apply(train, 1, function(x) sqrt(sum((x - 
            test[i, ])^2))), ties.method = "random") %in% seq(k)
        vote = cl[idx]
        res = c(res, factor(names(which.max(table(vote))), levels = levels(clf), 
            labels = levels(clf)))
        plot(rbind(train, test), type = "n", main = "Demonstration for KNN Classification", 
            xlab = expression(italic(X)[1]), ylab = expression(italic(X)[2]))
        points(train, col = "blue", pch = unclass(clf))
        points(test, col = "red", pch = "?")
        points(test[i, 1], test[i, 2], col = "white", pch = "?")
        points(test[i, 1], test[i, 2], col = "red", pch = "?", 
            cex = 2)
        legend("topleft", legend = levels(clf), pch = seq_along(levels(clf)), 
            bty = "n", y.intersp = 1.3, cex = 0.85)
        legend("bottomleft", legend = c("training set", "test set"), 
            fill = c("blue", "red"), bty = "n", y.intersp = 1.3, 
            cex = 0.85)
        if (saveANI) 
            savePNG(n = j)
        j = j + 1
        Sys.sleep(interval)
        segments(train[, 1], train[, 2], test[i, 1], test[i, 
            2], lty = "dashed", col = "gray")
        if (saveANI) 
            savePNG(n = j)
        j = j + 1
        Sys.sleep(interval)
        bd = train[idx, 1:2]
        polygon(bd[chull(bd), ], density = 10, col = "green")
        if (saveANI) 
            savePNG(n = j)
        j = j + 1
        Sys.sleep(interval)
        points(test[i, 1], test[i, 2], col = "white", pch = "?", 
            cex = 2)
        points(test[i, 1], test[i, 2], col = "red", pch = unclass(res)[i], 
            cex = 3, lwd = 2)
        if (saveANI) 
            savePNG(n = j)
        j = j + 1
        Sys.sleep(interval)
        jj = jj + 1
    }
    invisible(NULL) 
}

