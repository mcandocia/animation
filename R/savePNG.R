`savePNG` <- function(outdir = tempdir(), n = 1, width = 480, 
    height = 480, ...) {
    if (capabilities("png")) {
        if (!is.null(dev.list())) {
            from = dev.cur()
            if (exists("ANIenv", envir = .GlobalEnv)) {
                assign("ht", height, envir = get("ANIenv", envir = .GlobalEnv))
                assign("nmax", n, envir = get("ANIenv", envir = .GlobalEnv))
                outdir = file.path(dirname(get(".ani.file", envir = get("ANIenv", 
                  envir = .GlobalEnv))), "images")
            }
            png(paste(file.path(outdir, n), ".png", sep = ""), 
                width = width, height = height, unit = "px", 
                ...)
            to = dev.cur()
            dev.set(from)
            dev.copy(which = to)
            dev.off(to)
        }
        else {
            warning("There are no available graphical devices opened!")
        }
    }
    else {
        stop("PNG device not available!")
    }
    invisible(NULL)
} 
