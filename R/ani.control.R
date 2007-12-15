`ani.control` <- function(saveANI = FALSE, interval = 1, 
    nmax = 50, width = 480, height = 480, ...) {
    if (interval < 0) 
        stop("Time interval must be positive!")
    if (nmax < 0) 
        stop("Maximum number of animation frames must be positive!")
    if (exists("ANIenv", envir = .GlobalEnv))
        assign('interval', interval, envir = ANIenv) 
    list(saveANI = saveANI, interval = interval, nmax = nmax, 
        width = width, height = height, ...)
} 
