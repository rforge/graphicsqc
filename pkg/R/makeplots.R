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
#files<-plotExpr(c("plot(1:10)","plot(4:40)","x<-3","plot(2:23)"),c("pdf","ps"),
# "test","./testdir")
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
                       
    # First split expression up by ";" in case multiple expressions are stacked
    # with each other
    if (is.list(expr))
        expr <- unlist(expr)
    
    # Testing filetype is valid
    filetype <- getValidFiletypes(filetype)
    fileExtension <- paste(".", filetype, sep = "")
    # "ps" is changed to "postscript" so it can be used
    # for a function call at do.call
    filetype <- gsub("^ps$", "postscript", filetype)

    # Testing file prefix is valid
    # Prefix must be length 1
    if (length(prefix) == 1 || length(prefix) == length(expr)) {
        prefix <- as.character(prefix)
    } else {
        stop("prefix must be length 1 or same as expr", call. = FALSE)
    }
    
    # Testing valid path
    if (length(path) > 1) {
        warning("object ", sQuote("path"), " has more than one ",
                "element: only the first used in ", sQuote("path"), 
                call. = FALSE)
        path <- path[1]
    }
    wd <- getwd()
    on.exit(setwd(wd))
    if (is.null(path)) {
        warning("no path given: the path has been set to your current working",
                " directory", call. = FALSE)
        path <- wd
    } else {
        path <- makeDir(path)
    }
    setwd(path)
    
    ## remove (all) possible filenames we are going to make?

    # PAUL:  NO!  just fail if any possible overwrites exist.
    # check for overwrite as path/prefix*.filetype
    
    #numFiles (of each format, not the total)

    # PAUL:  just use %d

    numFiles <- length(expr)
    filenameFormat <- paste(prefix, "-%0", nchar(numFiles), "d", sep = "")

    # PAUL: loop over formats NOT exprs
    
    for (i in 1:length(filetype)) {
        do.call(filetype[i], list(paste(filenameFormat, fileExtension[i],
                                  sep = ""), onefile = FALSE))
        lapply(expr, evalPlotCode)
    }

    # result <- lapply(filetype, evalPlotCode, expr)
    
    graphics.off()
    
    
    ## now to find out what files we made!

    # We only created files if none already existed
    
    ## . vs \.
    pattern <- paste("^", prefix, "-", "[0-9]+[.](",
                   paste(fileExtension, collapse = "|"),
                   ")", sep = "")
    # PAUL:  JUST a list.files() call
    filenames <- grep(pattern, list.files(path), value = TRUE)
    
    
    ## we don't actually make these filenames.. they're just everything(minus
    ## multiplots) possible we _can_ make from given expr
    #filenames <- sprintf(filenameFormat, 1:numFiles)
    #filenames <- paste(rep(filenames, length(fileExtension)),
    #                   rep(fileExtension, each = length(filenames)), sep = "")

    # PAUL:  WRITE THE filenames AND the errors and warnings to a file
    
    invisible(filenames)
}

# --------------------------------------------------------------------
#
# evalPlotCode()
#
# --------------------------------------------------------------------


# PAUL:  just record the error message in the log
#        also record warnings
#        AS AN R OBJECT
"evalPlotCode" <- function(expr) {
    tryCatch(eval(parse(text = expr)), 
             error = function(e) { 
                         paste("Error in", expr, ":",
                               geterrmessage())
                     }, 
             warning = function(w) {
                           paste("Warning in", expr, 
                                 ":", conditionMessage(w))
                       })
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
           warning("Sorry, BMP format only supported in Windows",
                   call. = FALSE)
       }
       warning("invalid filetype(s) given: ", 
               paste(dQuote(filetypes[invalidTypes]), collapse = ", "),
               " ignored", call. = FALSE)
    }
    
    if (length(filetypes[!invalidTypes]) > 0) {
        return(filetypes[!invalidTypes])
    } else {
        stop("No valid filetypes given", call. = FALSE)
    }
}

# --------------------------------------------------------------------
#
# makeDir()
#
# --------------------------------------------------------------------
makeDir <- function(path, showWarnings = FALSE) {
    result <- dir.create(path, showWarnings)
        
    if (!result) {

        # PAUL: THIS SHOULD JUST FAIL !!!
        isdir <- file.info(path)$isdir

        if ((is.na(isdir)) || (!isdir)) {
            warning("failed to create the output directory ", 
                    sQuote(path), call. = FALSE)
            path <- getwd()
            warning("the output directory has been set to your current ", 
                    "working directory ", sQuote(path), call. = FALSE)
        }
    }
    return(path)
}

