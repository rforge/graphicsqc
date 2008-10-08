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
    names(evalResult) <- filetype
    # ---------- Get info of results ----------
    # (We only created files if none already existed)
    blankImageSizes <- getBlankImageSizes()
    if (any(is.na(blankImageSizes))) {
        generateBlankImages()
        blankImageSizes <- getBlankImageSizes()
        if (any(is.na(blankImageSizes))) {
            warning("blank image details could not be obtained, so blank ",
                    "files will not be removed")
        }
    }

    filenames <- list.files(getwd(), filenamePattern)
    # Remove blanks
    wereRemoved <- sapply(filenames, removeIfBlank, blankImageSizes,
                          USE.NAMES = FALSE)
    if (any(!wereRemoved)) {
           warning("some blank images could not be removed")
    }
    filenames <- list.files(getwd(), filenamePattern) ##full.names = TRUE
    plots <- lapply(filetype,
                function(filetype) {
                    plots <- grep(filetype, filenames, value = TRUE)
                    if (length(plots) > 0) plots else NULL
                })
    names(plots) <- filetype
    lapply(filetype, function(type)
                         evalResult[[type]]["plot"] <<- list(plots[[type]]))

    info <- list("OS" = .Platform$OS.type, "Rver" =
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call(1)), collapse = ""),
                ## at some point deparse(width.cutoff) might need to be raised
                 "directory" = getwd(), "logFilename" =
                 paste(prefix, "-log.xml", sep = ""))
                 # using getwd() here forces expansion
                 # of directory (ie, expands "./" or "~/")
    ## Write all errors (i.e. for every filetype), or just one set?
    results <- list("info" = info, "plots" = evalResult)

    writeXmlPlotExprLog(results)
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
        warning("the given path has more than one element: only",
                " the first used in ", dQuote(path[1]), call. = FALSE)
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
    tryCatch(withCallingHandlers(eval(if (is.language(expr)) expr else
                                                         parse(text = expr)),
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
    return(list("warnings" = warns, "error" = error))
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
# plotFile()
#
# --------------------------------------------------------------------
plotFile <- function(filename, # character vector
                             # R expression(s)
                     filetype = NULL, # character vector
                                        # (valid) file formats
                     prefix = basename(filename), # char length 1
                                      # file prefix
                     path = NULL, # char length 1
                                   # directory to create files in
                     clear = FALSE
                     ) {
    ## Test if files exist first?
   ### If filename has .Platform$file.sep in it then the prefix HAS to be diff
    if (length(filename) != length(prefix)) {
        stop(sQuote(prefix), " must be the same length as ", sQuote(filename))
    }
    if (length(grep(.Platform$file.sep, prefix)) > 0) {
        stop(sQuote(prefix), " cannot contain the system file separator")
    }
    path <- getValidPath(path)
    expr <- lapply(filename, readLines)
    fileMapplyResult <- mapply(plotExpr, expr = expr, prefix = prefix,
                              MoreArgs = list(filetype = filetype, path = path,
                              clear = clear), SIMPLIFY = FALSE)
    names(fileMapplyResult) <- NULL
    if (length(prefix) > 1) {
        # No warning - there is a note in the help file
        filePrefix <- prefix[1]
    } else {
        filePrefix <- prefix
    }
    
    path <- normalizePath(path.expand(path))
    info <- list("OS" = .Platform$OS.type, "Rver" =
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call(1)), collapse = ""),
                 "directory" = path,
                 "logFilename" = paste(filePrefix, "-fileLog.xml", sep = ""))
    
    # Note: plotExpr XML file gets written in the call to plotExpr
    writeXmlPlotTypeLog(prefix, info, "file")
    fileResult <- list("info" = info, "results" = fileMapplyResult)
    class(fileResult) <- c("qcPlotFileResult")
    fileResult
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
    ## can also check if it's a function? .. is.function(match.fun(..
    if (length(fun) != length(prefix)) {
        stop(sQuote("prefix"), " must be the same length as ", sQuote("fun"))
    }
    # Check that the prefixes are unique (otherwise we will die half-way
    # through because we will try to overwrite)
    if (any(duplicated(prefix))) {
        stop("all values for ", sQuote("prefix"), "must be unique")
    }
    path <- getValidPath(path)
    if (!is.character(fun)) {
        fun <- deparse(substitute(fun))
        prefix <- fun
    }
    funs <- paste("example(", fun, ", echo = FALSE, setRNG = TRUE)", sep = "")
    funMapplyResult <- mapply(plotExpr, expr = funs, prefix = prefix,
                              MoreArgs = list(filetype = filetype, path = path,
                              clear = clear), SIMPLIFY = FALSE)
    names(funMapplyResult) <- NULL
    if (length(prefix) > 1) {
        # No warning - there is a note in the help file
        filePrefix <- prefix[1]
    } else {
        filePrefix <- prefix
    }
    
    path <- normalizePath(path.expand(path))
    info <- list("OS" = .Platform$OS.type, "Rver" =
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call(1)), collapse = ""),
                 "directory" = path,
                 "logFilename" = paste(filePrefix, "-funLog.xml", sep = ""))
    
    writeXmlPlotTypeLog(prefix, info, "fun")
    funResult <- list("info" = info, "results" = funMapplyResult)
    class(funResult) <- c("qcPlotFunResult")
    funResult
}

# --------------------------------------------------------------------
#
# plotPackage()
#
# --------------------------------------------------------------------
plotPackage <- function(package) {
    ## take 'best effort' approach? try get tests/demo if can and use them..
    if (!do.call(require, list(package))) {
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
    pdf(paste(tempDir, "blankPDF.pdf", sep = .Platform$file.sep),
        onefile = FALSE)
    dev.off()
    postscript(paste(tempDir, "blankPS.ps", sep = .Platform$file.sep),
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
    sizes <- file.info(paste(tempdir(), c("blankPDF.pdf", "blankPS.ps"),
                             sep = .Platform$file.sep))[,1]
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
        if (!file.remove(filename)) {
            return(FALSE)
        }
    }
    return(TRUE)
}

# --------------------------------------------------------------------
#
# print.qcPlotExprResult()
#
# --------------------------------------------------------------------
print.qcPlotExprResult <- function (obj) {
    cat("plotExpr Result:\n")
    cat("Call:\t", obj[["info"]][["call"]], "\n")
    cat("R version:\t", obj[["info"]][["Rver"]], "\n")
    cat("Directory:\t", obj[["info"]][["directory"]], "\n")
    cat("Filename:\t", obj[["info"]][["logFilename"]], "\n")
    cat("Formats:\n")
    for (format in names(obj[["plots"]])) {
        if (is.null(obj[["plots"]][[format]][["plot"]])) {
            plots = "none"
        } else {
            plots = obj[["plots"]][[format]][["plot"]]
        }
        cat(" ", format, ":\tPlots: ")
        cat(plots, sep = ", ")
        cat("\n")

        if (!is.null(obj[["plots"]][[format]][["warnings"]])) {
            cat("\tWarnings: \t")
            cat(obj[["plots"]][[format]][["warnings"]], sep = "\n\t\t\t")
            cat("\n")
        }
        if (!is.null(obj[["plots"]][[format]][["error"]])) {
            cat("\tError: ", obj[["plots"]][[format]][["error"]], "\n")
        }
    }
}


# --------------------------------------------------------------------
#
# notYetImplemented()
#
# --------------------------------------------------------------------
notYetImplemented <- function() {
    stop("sorry, that function is not yet implemented")
}
