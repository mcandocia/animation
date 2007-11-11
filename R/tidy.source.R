`tidy.source` <-
function(file) {
    exprs = parse(file)
    for (i in 1:length(exprs)) {
        dep = paste(deparse(exprs[i]), collapse = "\n")
        dep = substring(dep, 12, nchar(dep) - 1)
        cat(dep, "\n")
    }
}

