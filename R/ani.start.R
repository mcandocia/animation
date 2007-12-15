`ani.start` <-
function(outdir = tempdir(), filename = "index", 
    extension = "htm", withprompt = "ANI> ", Title = "Animated Statistics Using R") {
    assign("ANIenv", new.env(parent = .GlobalEnv), envir = .GlobalEnv)
    assign("oldprompt", getOption("prompt"), envir = get("ANIenv", 
        envir = .GlobalEnv))
    options(prompt = withprompt)
    op = getwd()
    setwd(outdir)
    if (!file.exists("images")) {
        dir.create("images")
    }
    else {
        file.remove(list.files("images"))
    }
    file.copy(system.file("js", "ANI.css", package = "animation"), 
        "ANI.css", overwrite = TRUE)
    file.copy(system.file("js", "FUN.js", package = "animation"), 
        "FUN.js", overwrite = TRUE)
    setwd(op)
    .ani.file = file.path(outdir, paste(filename, ".", extension, 
        sep = ""))
    assign(".ani.file", .ani.file, envir = get("ANIenv", envir = .GlobalEnv))
    cat("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n<title>", 
        file = .ani.file)
    cat(Title, file = .ani.file, append = TRUE)
    cat("</title>\n<link href=\"ANI.css\" rel=\"stylesheet\" type=\"text/css\" />\n<script language=\"JavaScript\" type=\"text/javascript\" src=\"FUN.js\"></script>\n<script language=\"JavaScript\" type=\"text/javascript\" src=\"ARG.js\"></script>\n<meta name=\"Author\" content=\"Yihui XIE\" />\n</head>\n<body>\n<div align=\"center\" id=\"divDemo\">\n<div id=\"loading\">loading animation frames...</div>\n<div id=\"divPreload\">\n<script language=\"JavaScript\" type=\"text/javascript\">\n", 
        file = .ani.file, append = TRUE)
}

