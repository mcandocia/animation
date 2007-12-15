`knn.ani` <- function(train, test, cl, k = 10, interact = FALSE, 
    control = ani.control(), ...) {
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
    if (missing(train)) {
        train = matrix(c(rnorm(40, mean = -1), rnorm(40, mean = 1)), 
            ncol = 2, byrow = TRUE)
        cl = rep(c("first class", "second class"), each = 20)
    }
    if (missing(test)) 
        test = matrix(rnorm(20, mean = 0, sd = 1.2), ncol = 2)
    train <- as.matrix(train)
    if (interact) {
        plot(train, main = "Choose test set points", pch = unclass(as.factor(cl)), 
            col = "blue")
        lct = locator(n = control$nmax, type = "p", pch = "?", 
            col = "red")
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
    nte = nrow(test)
    clf = as.factor(cl)
    res = NULL
    j = 1
    jj = 1
    for (i in 1:nte) {
        if (jj > control$nmax) 
            break
        plot(rbind(train, test), type = "n", main = "Demonstration for kNN Classification", 
            xlab = expression(italic(X)[1]), ylab = expression(italic(X)[2]))
        points(train, col = "blue", pch = unclass(clf))
        if (i < nte) 
            points(test[(i + 1):nte, 1], test[(i + 1):nte, 2], 
                col = "red", pch = "?")
        if (i > 1) 
            points(test[1:(i - 1), 1], test[1:(i - 1), 2], col = "red", 
                pch = unclass(res), cex = 2)
        points(test[i, 1], test[i, 2], col = "red", pch = "?", 
            cex = 2)
        legend("topleft", legend = levels(clf), pch = seq_along(levels(clf)), 
            bty = "n", y.intersp = 1.3, cex = 0.85)
        legend("bottomleft", legend = c("training set", "test set"), 
            fill = c("blue", "red"), bty = "n", y.intersp = 1.3, 
            cex = 0.85)
        if (control$saveANI) 
            savePNG(n = j, width = control$width, height = control$height)
        j = j + 1
        Sys.sleep(control$interval)
        idx = rank(apply(train, 1, function(x) sqrt(sum((x - 
            test[i, ])^2))), ties.method = "random") %in% seq(k)
        vote = cl[idx]
        res = c(res, factor(names(which.max(table(vote))), levels = levels(clf), 
            labels = levels(clf)))
        segments(train[, 1], train[, 2], test[i, 1], test[i, 
            2], lty = "dashed", col = "gray")
        if (control$saveANI) 
            savePNG(n = j, width = control$width, height = control$height)
        j = j + 1
        Sys.sleep(control$interval)
        bd = train[idx, 1:2]
        if (k > 1) {
            polygon(bd[chull(bd), ], density = 10, col = "tomato")
        }
        else {
            points(bd[1], bd[2], col = "red", pch = unclass(clf)[idx], 
                cex = 2, lwd = 2)
        }
        if (control$saveANI) 
            savePNG(n = j, width = control$width, height = control$height)
        j = j + 1
        Sys.sleep(control$interval)
        points(test[i, 1], test[i, 2], col = "white", pch = "?", 
            cex = 2)
        points(test[i, 1], test[i, 2], col = "red", pch = unclass(res)[i], 
            cex = 3, lwd = 2)
        if (control$saveANI) 
            savePNG(n = j, width = control$width, height = control$height)
        j = j + 1
        Sys.sleep(control$interval)
        jj = jj + 1
    }
    invisible(levels(clf)[res])
} 
