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
    wd <- getwd()
    on.exit(setwd(wd))
    # Get valid path
    path <- getValidPath(path)
    
    # Check we are not going to overwrite any files
    filenamePattern <- paste("^(", prefix, "-", "[0-9]+[.](",
                   paste(filetype, collapse = "|"),
                   ")|", prefix, "-log[.]xml)$", sep = "")
                   # ie "^(prefix-[0-9]+[.](pdf|ps)|prefix-log[.]xml)$"
                   # Will match (prefix = "prefix"; filetype = c("pdf", "ps")):
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
    # ---------- Get info of results ----------
    # (We only created files if none already existed)
    filenames <- list.files(getwd(), filenamePattern, full.names = TRUE)
    blankImageSizes <- getBlankImageSizes()
    if (any(is.na(blankImageSizes))) {
        generateBlankImages()
        blankImageSizes <- getBlankImageSizes()
        if (any(is.na(blankImageSizes))) {
            warning("blank image details could not be obtained, so blank ",
                    "files will not be removed")
        }
    }
    # Remove blanks
    wereRemoved <- unlist(lapply(filenames, removeIfBlank, blankImageSizes))
    if (any(!wereRemoved)) {
           warning("some blank images could not be removed")
    }
    filenames <- list.files(getwd(), filenamePattern)
    if (length(filenames) == 0) {
        filenames <- NULL
    }
    
    info <- list("OS" = .Platform$OS.type, "Rver" = 
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call(1)), collapse = ""),
                ## at some point deparse(width.cutoff) might need to be raised
                 "filetype" = filetype, "directory" = getwd())
                 #using getwd() here forces expansion
                 #of directory (ie, expands "./" or "~/")
    ## Write all errors (i.e. for every filetype), or just one set?
    results <- list("filenames" = filenames, "warnings" =
                    evalResult[[1]][["warnings"]], "errors" =
                    evalResult[[1]][["errors"]], "info" = info)

    writeXmlPlotExprLog(results, prefix)
    class(results) <- "qcPlotExprResult"
    # ---------- return results ----------
    invisible(results)
}

# --------------------------------------------------------------------
#
# getValidPath()
#
# --------------------------------------------------------------------
getValidPath <- function(path) {
    if (length(path) == 0) {
        warning("no path given: the path has been set to your current working",
                " directory", call. = FALSE)
        path <- getwd()
    } else if (length(path) == 1) {
        isDir <- file.info(path)$isdir
        isCreated <- TRUE
        if (is.na(isDir)) {
            isCreated <- dir.create(path, showWarnings = FALSE)
        } else if (!isDir || !isCreated) {
            stop("could not create directory ", dQuote(path), call. = FALSE)
        }
    } else {
        warning("object ", sQuote("path"), " has more than one element: only",
                " the first used in ", sQuote("path"), call. = FALSE)
        path <- getValidPath(path[1])
    }
    path
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
    warns <- NULL
    # Reset last error message to ""
    tryCatch(stop(), error = function(e) {})
    tryCatch(withCallingHandlers(eval(parse(text = expr)),
                    warning = function(w) { warns <<- c(warns, 
                                            paste("Warning in evalPlotCode :",
                                                  conditionMessage(w)));
                    invokeRestart("muffleWarning") }), error = function(e) {})
    error <- geterrmessage() # There can only be one error as we stop
                             # evaluating when we hit an error
    if (error == "") {
        error <- NULL
    } else {
        error <- paste("Error in evalPlotCode :", error)
    }

    dev.off()
    return(list("warnings" = warns, "errors" = error))
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
# writeXmlPlotExprLog()
#
# --------------------------------------------------------------------
"writeXmlPlotExprLog" <- function(results, prefix) {
    library(XML) ## might not run    
    ## Should be a listToXml function.. this can still be more efficient
    xmlResults <- xmlOutputDOM(tag="qcPlotExprResult")
    xmlResults$addTag("info", close = FALSE)
     xmlResults$addTag("OS", results[["info"]][["OS"]])
     xmlResults$addTag("Rver", results[["info"]][["Rver"]])
     xmlResults$addTag("date", results[["info"]][["date"]])
     xmlResults$addTag("call", close = FALSE)
      xmlResults$addCData(results[["info"]][["call"]])
     xmlResults$closeTag() # call
     lapply(results[["info"]][["filetype"]], xmlResults$addTag,
                                                              tag="filetype")
     xmlResults$addTag("directory", results[["info"]][["directory"]])
    xmlResults$closeTag() # info
    xmlResults$addTag("warnings", close = FALSE)
     lapply(results[["warnings"]], xmlResults$addTag, tag="warning")
    xmlResults$closeTag() # warnings
    xmlResults$addTag("errors", close = FALSE)
     lapply(results[["errors"]], xmlResults$addTag, tag="error")
    xmlResults$closeTag() # errors
    xmlResults$addTag("filenames", close = FALSE)
     lapply(results[["filenames"]], xmlResults$addTag, tag="filename")
    xmlResults$closeTag() # filenames
    saveXML(xmlResults, paste(prefix, "-log.xml", sep = ""))
}

# --------------------------------------------------------------------
#
# writeXmlPlotFunLog()
#
# --------------------------------------------------------------------
"writeXmlPlotFunLog" <- function(exprPrefix, path, filePrefix) {
    library(XML) ## might not run    

    xmlResults <- xmlOutputDOM(tag="qcPlotFunResult")
     lapply(paste(path, exprPrefix, "-log.xml", sep = ""), xmlResults$addTag,
                                                       tag="qcPlotExprResult")
    saveXML(xmlResults, paste(path, filePrefix, "-funLog.xml", sep = ""))
}

# --------------------------------------------------------------------
#
# plotFile()
#
# --------------------------------------------------------------------
plotFile <- function(filename, # character vector
                             # R expression(s)
                       filetype = NULL, # character vector
                                        # (valid) file formats
                       prefix = filename, # char length 1
                                      # file prefix
                       path = NULL, # char length 1
                                   # directory to create files in
                       clear = FALSE
                       ) {
    expr <- lapply(filename, readLines)
    mapply(plotExpr, expr = expr, prefix = prefix,
          MoreArgs = list(filetype = filetype, path = path, clear = clear))
    ##Still need to write XML?
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
    path <- getValidPath(path)
    funs <- paste("example(", fun, ", echo = FALSE, setRNG = TRUE)", sep = "")
    funMapplyResult <- mapply(plotExpr, expr = funs, prefix = prefix,
          MoreArgs = list(filetype = filetype, path = path, clear = clear))
    funResults <- vector("list", dim(funMapplyResult)[2])
    for (i in 1:length(funResults)) {
        funResults[[i]] <- funMapplyResult[,i]
        class(funResults[[i]]) <- c("qcPlotExprResult")
    }
    writeXmlPlotFunLog(prefix, paste(getAbsolutePath(path),
                    .Platform$file.sep, sep = ""), paste(fun, collapse = "-"))
    class(funResults) <- c("qcPlotFunResult")
    invisible(funResults)
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
    notYetImplemented()
}

# --------------------------------------------------------------------
#
# generateBlankImages()
#
# --------------------------------------------------------------------
generateBlankImages <- function() {
    tempDir <- tempdir()
    # Only pdf and ps blank images are made as the filesize for bmp and png
    # blanks are 0
    pdf(paste(tempDir, .Platform$file.sep, "blankPDF.pdf", sep = ""),
        onefile = FALSE)
    dev.off()
    postscript(paste(tempDir, .Platform$file.sep, "blankPS.ps", sep = ""),
               onefile = FALSE)
    dev.off()
    invisible()
}

# --------------------------------------------------------------------
#
# getBlankImageSizes()
#
# --------------------------------------------------------------------
getBlankImageSizes <- function() {
    sizes <- file.info(paste(tempdir(), .Platform$file.sep,
              c("blankPDF.pdf", "blankPS.ps"), sep=""))[,1]
    names(sizes) <- c("pdf", "ps")
    sizes
}

# --------------------------------------------------------------------
#
# removeIfBlank()
#
# --------------------------------------------------------------------
removeIfBlank <- function(filename, blankImageSizes) {
    filesize <- file.info(filename)[,1]
    if (filesize == 0 || length(grep(".*[.]pdf$", filename)) > 0 &&
        filesize == blankImageSizes["pdf"] ||
        length(grep(".*[.]ps$", filename)) > 0 && filesize ==
                                               blankImageSizes["ps"]) {
        if(!file.remove(filename)) {
            return(FALSE)
        }
    }
    return(TRUE)
}

# --------------------------------------------------------------------
#
# getAbsolutePath()
#
# --------------------------------------------------------------------
getAbsolutePath <- function(path) {
    wd <- getwd()
    on.exit(setwd(wd))
    setwd(path)
    getwd()
}

# --------------------------------------------------------------------
#
# notYetImplemented()
#
# --------------------------------------------------------------------
notYetImplemented <- function() {
    stop("sorry, that function is not yet implemented")
}
