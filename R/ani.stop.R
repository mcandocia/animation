`ani.stop` <- function(footer = TRUE, autobrowse = TRUE) {
    if (exists("ANIenv", envir = .GlobalEnv)) {
        if (exists("ht", envir = get("ANIenv", envir = .GlobalEnv)) & 
            exists("nmax", envir = get("ANIenv", envir = .GlobalEnv))) {
            .ani.file = get(".ani.file", envir = get("ANIenv", 
                envir = .GlobalEnv))
            cat(paste("var nmax = ", get("nmax", envir = get("ANIenv", 
                envir = .GlobalEnv)), "\n var ht = ", get("ht", 
                envir = get("ANIenv", envir = .GlobalEnv)), "\n", 
                sep = ""), file = .ani.file, append = TRUE)
            cat("for(i = 1; i <= nmax; i++){\n\tdocument.write(\"<div id=\\\"divPreload\" + i + \"\\\"><img src=\\\"images/\" + i + \".png\\\" /></div>\")\n}\n</script>\n</div>\n</div>\n<div style=\"border: solid 1px #FF0000; margin-top: 20px; height: 20px;\" title=\"Speed\">\n<div id=\"percent\" style=\"width: 80%\">80%</div>\n</div>\n<div id=\"divControl\">\n<input type=\"button\" disabled=\"disabled\" id=\"btnBegin\" onclick=\"displayImage()\" value=\"Begin\"/>\n<input type=\"button\" disabled=\"disabled\" id=\"btnFaster\" onclick=\"fasterImage(-0.1)\" value=\"Faster\"/>\n<input type=\"button\" disabled=\"disabled\" id=\"btnSlower\" onclick=\"fasterImage(0.1)\" value=\"Slower\"/>\n<input type=\"button\" disabled=\"disabled\" id=\"btnStop\" onclick=\"stopImage()\" value=\"Stop \"/> \nTime Interval: <label id=\"lbl\">", 
                file = .ani.file, append = TRUE)
            cat(paste(get("interval", envir = get("ANIenv", envir = .GlobalEnv)), 
                "</label> seconds; <label id=\"lblImage\"></label>\n</div>\n", 
                sep = ""), file = .ani.file, append = TRUE)
            if (footer) 
                cat(paste("<div id=\"footer\">Created by package \"animation\" written by <a href=\"http://www.yihui.name/\" target=\"_blank\">Yihui XIE</a>.<br>", 
                  Sys.time(), "</div>", sep = ""), file = .ani.file, 
                  append = TRUE)
            cat("</body>\n</html>", file = .ani.file, append = TRUE)
            options(prompt = get("oldprompt", envir = get("ANIenv", 
                envir = .GlobalEnv)))
            if (autobrowse) 
                on.exit(browseURL(paste("file://", .ani.file, 
                  sep = "")), add = TRUE)
            cat("HTML animation page created: ", normalizePath(.ani.file), 
                "\n")
            on.exit(rm("ANIenv", envir = .GlobalEnv), add = TRUE)
        }
    }
    else {
        warning("It seems that you haven't started an animation yet!")
    }
} 
