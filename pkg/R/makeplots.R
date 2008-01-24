# --------------------------------------------------------------------
# makeplots.R
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#
# plotExpr()
#
# plotExpr will take an R expression (or expressions) and produce
# plots in specific file formats of the intended plot.
#
# ALWAYS produce a file containing index of files created
#
# MUST always work (=> tryCatch)
#
# Return value is record of files created
# 
#
#  _______-------======= TODO: FIND ##'s =======-------_______
#
#files<-plotExpr(c("plot(1:10)","plot(4:40)","x<-3","plot(2:23)"),
#                c("pdf","ps"),"test","testdir")
# --------------------------------------------------------------------
"plotExpr" <- function(expr, # character vector
                             # R expression(s)
                             # May be a list BUT if it is
                             # we just flatten it to a vector.
                       filetype = NULL, # character vector
                                        # (valid) file formats
                       prefix = NULL, # char length 1
                                      # file prefix
                       path = NULL # char length 1
                                   # directory to create files in
                       ) {
### on.exit(unlink( all files we (could've) made )) ?????
    if (is.list(expr))
        expr <- unlist(expr)
    
    # Testing filetype is valid
    filetype <- getValidFiletypes(filetype)
    fileExtension <- paste(".", filetype, sep = "")

    # Testing file prefix is valid
    # Prefix must be length 1
    if (length(prefix) == 1) {
        prefix <- as.character(prefix)
    } else {
        stop(sQuote("prefix"), " must be a character vector of length 1")
    }
    
    # Testing valid path
    wd <- getwd()
    on.exit(setwd(wd))
    if (length(path) == 0) {
        warning("no path given: the path has been set to your current working",
                " directory")
        path <- wd
    } else if (length(path) == 1) {
        path <- setDir(path)
    } else {
        warning("object ", sQuote("path"), " has more than one ",
                "element: only the first used in ", sQuote("path"))
        path <- path[1]
    }
    
    # Check we are not going to overwrite any files
    filenamePattern <- paste("^(", prefix, "-", "[0-9]+[.](",
                   paste(filetype, collapse = "|"),
                   ")|", prefix, "-log[.]xml)$", sep = "")
                   # ie "^(prefix-[0-9]+[.](pdf|ps)|prefix-log[.]xml)$"
                   # Will match (prefix = "prefix"; filetype = c("pdf", "ps"):
                   # prefix-123.pdf, prefix-log.xml, prefix-1.ps
                   # Will NOT match:
                   # prefix-log.pdf, prefix-123.xml, prefix-.ps
    currentFilenames <- list.files(path, filenamePattern)
    if (length(currentFilenames) > 0) {
        stop("files of intended filename already exist in ", sQuote("path"),
             call. = FALSE)
    }
    
    filenameFormat <- paste(prefix, "-%d", sep = "")
    setwd(path)
    result <- lapply(filetype, evalPlotCode, expr, filenameFormat)    
    graphics.off()
    
    # ---------- Get info of results ----------
    # (We only created files if none already existed)
    filenames <- list.files(getwd(), filenamePattern)    
    warnings <- grep("^Warning", result)
    errors <- grep("^Error", result)
    info <- list("OS" = .Platform$OS.type, "Rver" = 
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call()), collapse = ""),
                ## at some point deparse(width.cutoff) might need to be raised
                 "filetype" = filetype, "directory" = getwd())
                 #using getwd() here forces expansion
                 #of directory (ie, expands "./")
    results <- list("filenames" = filenames, "warnings" = warnings,
                    "errors" = errors, "info" = info)

    writeXmlLogFile(results, prefix)
    class(results) <- "qcPlotResult"
    # ---------- return results ----------
    invisible(results)
}

# --------------------------------------------------------------------
#
# evalPlotCode()
#
# --------------------------------------------------------------------
"evalPlotCode" <- function(filetype, expr, filenameFormat) { 
    if (filetype == "ps") {
        fileExtension <- ".ps"
        filetype <- "postscript"
    } else {
        fileExtension <- paste(".", filetype, sep = "")
    }
    if (any(filetype == c("pdf", "postscript"))) {
        do.call(filetype, list(paste(filenameFormat, fileExtension,
                                     sep = ""), onefile = FALSE))
    } else {
        do.call(filetype, list(paste(filenameFormat, fileExtension,
                                     sep = "")))
    }
    ## withCallingHandlers is currently used because tryCatch will exit on
    ## warnings as well as errors, whereas withCallingHandlers can carry on
    ## after a warning
#    result <- withCallingHandlers(eval(parse(text = expr)), 
#             error = function(e) { 
#                         paste("Error in expr:", e) 
#                     }, 
#             warning = function(w) {
#                           paste("Warning in expr:", w)
#                       })

    ## First clear last.warning and option(error) ..
    withCallingHandlers(eval(parse(text = expr)),
                      warning = function(w){ invokeRestart("muffleWarning")})
    warnings <- warnings()   #a.k.a "last.warning" but pretty
    error <- geterrmessage() #can only be one error as we stop expr when
                             #we hit an error
    ## See "suppressedWarnings()"

    invisible(list("warnings" = warnings, "errors" = error)
}

# --------------------------------------------------------------------
#
# getValidFiletypes()
#
# --------------------------------------------------------------------
"getValidFiletypes" <- function(filetypes) {
    filetypes <- tolower(filetypes)
    validFiletypes <- c("pdf", "png", "ps", "bmp")
    if (.Platform$OS.type != "windows") {
        validFiletypes<-validFiletypes[-4]
    }
    
    # check for duplications
    if (any(duplicated(filetypes))) {
        warning("duplicated filetypes: ",
                paste(dQuote(filetypes[duplicated(filetypes)]),
                      collapse = ", ")
                , " duplication ignored", call. = FALSE)
        filetypes <- filetypes[!duplicated(filetypes)]
    }
    
    # check given filetypes against valid filetypes
    invalidTypes <- !filetypes %in% validFiletypes
    if (any(invalidTypes)) {
       if (any(filetypes[invalidTypes] %in% "bmp")) {
           warning("sorry, BMP format only supported in Windows",
                   call. = FALSE)
       }
       warning("invalid filetype(s) given: ", 
               paste(dQuote(filetypes[invalidTypes]), collapse = ", "),
               " ignored", call. = FALSE)
    }
    
    if (length(filetypes[!invalidTypes]) > 0) {
        return(filetypes[!invalidTypes])
    } else {
        stop("no valid filetypes given", call. = FALSE)
    }
}

# --------------------------------------------------------------------
#
# setDir()
#
# --------------------------------------------------------------------
"setDir" <- function(path, showWarnings = FALSE) {
    # Does dir exist? if yes, use it. else, make it. if fail, stop.
    isDir <- file.info(path)$isdir
    if (!isDir || is.na(isDir)) {
        isCreated <- dir.create(path, showWarnings)
        if (!isCreated) {
            stop("failed to create the output directory ", 
                    dQuote(path), " plotExpr failed" , call. = FALSE)
        }
    }
    return(path)
}

# --------------------------------------------------------------------
#
# writeXmlLogFile()
#
# --------------------------------------------------------------------
"writeXmlLogFile" <- function(results, prefix) {
    library(XML) ## might not run
    
    ## Should first check if a log file exists.. then can insert new filetype
    ## order(reverse(filenames)) ??
    xmlResults <- xmlOutputDOM(tag="qcPlotResult")
    xmlResults$addTag("info", close = FALSE)
     xmlResults$addTag("OS", results[["info"]][["OS"]])
     xmlResults$addTag("Rver", results[["info"]][["Rver"]])
     xmlResults$addTag("date", results[["info"]][["date"]])
     xmlResults$addTag("call", close = FALSE)
      xmlResults$addCData(results[["info"]][["call"]])
     xmlResults$closeTag() # call
     for (i in 1:length(results[["info"]][["filetype"]])) {
         xmlResults$addTag("filetype", results[["info"]][["filetype"]][i])
     }
     xmlResults$addTag("directory", results[["info"]][["directory"]])
    xmlResults$closeTag() # info
    xmlResults$addTag("warnings", close = FALSE)
     if (length(results[["warnings"]]) > 0) {
         for (i in 1:length(results[["warnings"]])) {
             xmlResults$addTag("warning", results[["warnings"]][i])
         }
     }
    xmlResults$closeTag() # warnings
    xmlResults$addTag("errors", close = FALSE)
     if (length(results[["errors"]]) > 0) {
         for (i in 1:length(results[["errors"]])) { ##apply?
             xmlResults$addTag("error", results[["errors"]][i])
         }
     }
    xmlResults$closeTag() # errors
    xmlResults$addTag("filenames", close = FALSE)
     if (length(results[["filenames"]]) > 0) {
         for (i in 1:length(results[["filenames"]])) {
             xmlResults$addTag("filename", results[["filenames"]][i])
         }
     }
    xmlResults$closeTag() # filenames
    saveXML(xmlResults, paste(prefix, "-log.xml", sep = ""))
}

# --------------------------------------------------------------------
#
# plotFile()
#
#
# --------------------------------------------------------------------
plotFile <- function(filename, # character vector
                             # R expression(s)
                       filetype = NULL, # character vector
                                        # (valid) file formats
                       prefix = NULL, # char length 1
                                      # file prefix
                       path = NULL # char length 1
                                   # directory to create files in
                       ) {
    expr <- lapply(filename, readLines)
    return(plotExpr(expr, filetype, prefix, path))
}

# --------------------------------------------------------------------
#
# plotFunction()
# ## setRNG?
# --------------------------------------------------------------------
plotFunction <- function(fun, # character vector
                             # R expression(s)
                       filetype = NULL, # character vector
                                        # (valid) file formats
                       prefix = fun, # char length 1
                                      # file prefix
                       path = NULL # char length 1
                                   # directory to create files in
                       ) {
    ## if not paste, then something like... 
    # funs<-lapply(fun, function(x) { 
       #                 substitute(do.call(example,list(x))
       # (gets unusual behaviour without paste..?)
    funs <- paste("example(", fun, ", echo = FALSE, setRNG = TRUE)", sep = "")
    return(plotExpr(funs, filetype, prefix, path))
}

# --------------------------------------------------------------------
#
# plotPackage()
#
# --------------------------------------------------------------------
plotPackage <- function(package) {
    ## take 'best effort' approach? try get tests/demo if can and use them..
    if(!do.call(require, list(package))) {
        warning("failed to load package ", dQuote(package))
    } # now package is loaded
}
