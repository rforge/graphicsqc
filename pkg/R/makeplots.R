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
    filenamePattern <- paste("^", prefix, "-", "[0-9]+[.](",
                   paste(filetype, collapse = "|"),
                   ")$", sep = "")
    currentFilenames <- list.files(path, filenamePattern)
    if (length(currentFilenames) > 0) {
        stop("files of intended filename already exist in ", sQuote("path"),
             call. = FALSE)
    }
    
    filenameFormat <- paste(prefix, "-%d", sep = "")
    setwd(path)
    result <- lapply(filetype, evalPlotCode, expr, filenameFormat)    
    graphics.off()
    
    ## now to find out what files we made!

    # (We only created files if none already existed)
    filenames <- list.files(getwd(), filenamePattern)
    
    #filenames <- sprintf(filenameFormat, 1:numFiles)
    #filenames <- paste(rep(filenames, length(fileExtension)),
    #                   rep(fileExtension, each = length(filenames)), sep = "")

    # PAUL:  WRITE THE filenames AND the errors and warnings to a file
    # Stephen: ie write to prefix-0.pdf.txt + prefix-0.ps.txt? 
    # prefix-warnings..?
    
    results <- list("filenames" = filenames,"results" = result)
    # save(results, file = ... )
    invisible(results)
}

# --------------------------------------------------------------------
#
# evalPlotCode()
#
# --------------------------------------------------------------------


# PAUL:  just record the error message in the log
#        also record warnings
#        AS AN R OBJECT
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
    result <- withCallingHandlers(eval(parse(text = expr)), 
             error = function(e) { 
                         paste("Error in expr:", e)  #geterrmessage())
                     }, 
             warning = function(w) {
                           paste("Warning in expr:", w)
                       })
    invisible(result)
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
setDir <- function(path, showWarnings = FALSE) {
    ## Does dir exist? if yes, use it. else, make it. if fail, stop.
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


