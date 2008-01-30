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
                       path = NULL, # char length 1
                                   # directory to create files in
                       clear = FALSE # boolean, clear any files we might make
                       ) {
## what about these warnings not in evalPlotCode?
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
    # 'Clear' files we might make if we are told to
    if (length(clear) > 1) {
        warning(sQuote("clear"), " has more than one element: only the ",
                "first used")
        clear <- clear[1]
    }
    if (is.logical(clear) && !is.na(clear)) {
        if (clear && length(currentFilenames) > 0) {
            if (any(!file.remove(paste(path, currentFilenames,
                              sep = .Platform$file.sep)))) {
                stop("tried to clear but failed to delete files")
            }
        } else if (!clear && length(currentFilenames) > 0) {
            stop("files of intended filename already exist in ",
                 sQuote("path"), call. = FALSE)
        }
    } else {
        stop(sQuote("clear"), " must be either TRUE or FALSE")
    }
    
    filenameFormat <- paste(prefix, "-%d", sep = "")
    setwd(path)
    evalResult <- lapply(filetype, evalPlotCode, expr, filenameFormat)  
    graphics.off()
    
    # ---------- Get info of results ----------
    # (We only created files if none already existed)
    filenames <- list.files(getwd(), filenamePattern)
    info <- list("OS" = .Platform$OS.type, "Rver" = 
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call()), collapse = ""),
                ## at some point deparse(width.cutoff) might need to be raised
                 "filetype" = filetype, "directory" = getwd())
                 #using getwd() here forces expansion
                 #of directory (ie, expands "./")
    ## Write all errors (ie for every filetype), or just one set?
    results <- list("filenames" = filenames, "warnings" =
                    evalResult[[1]][["warnings"]], "errors" =
                    evalResult[[1]][["errors"]], "info" = info)

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
## Test: expr<-c("x<-3", "plot(x:8)", "warning(\"firstWarning\")",
#          "warning(\"secondWarning\")", "stop(\"end error\")",
#          "warning(\"NOTCALLED\")")
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
    assign("evalWarns", NULL, envir = globalenv())
    on.exit(rm(evalWarns, envir = globalenv()))
    # Reset last error message to ""
    tryCatch(stop(), error = function(e) {})
    tryCatch(withCallingHandlers(eval(parse(text = expr)),
                    warning = function(w) { assign("evalWarns", 
                    c(evalWarns, paste("Warning in evalPlotCode :",
                                       conditionMessage(w))), 
                    envir=globalenv());
                    invokeRestart("muffleWarning") }), error = function(e) {})
    error <- geterrmessage() # There can only be one error as we stop
                             # evaluating when we hit an error
    if (error == "") {
        error <- NULL
    } else {
        error <- paste("Error in evalPlotCode :", error)
    }

    dev.off()
    return(list("warnings" = evalWarns, "errors" = error))
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
                       path = NULL, # char length 1
                                    # directory to create files in
                       clear = FALSE) {
    ## if not paste, then something like... 
    # funs<-lapply(fun, function(x) { 
       #                 substitute(do.call(example,list(x))
       # (gets unusual behaviour without paste..?)
    funs <- paste("example(", fun, ", echo = FALSE, setRNG = TRUE)", sep = "")
    ## try one and look at the call.
    # ie a<-plotFunction(c("plot","lm"), "ps", path="./testdir")
    # then a[1:4] is one, and a[5:8] is the next
    # nested for loop rather than mapply?
    mapply(plotExpr, expr = funs, prefix = prefix,
           MoreArgs = list(filetype = filetype, path = path, clear = clear))
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

##hasBlanks()
##makeBlanks()


