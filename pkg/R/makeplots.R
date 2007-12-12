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
# source("~/graphicsqc/makeplots.R")
# --------------------------------------------------------------------
"plotExpr" <- function(expr, # character vector
                             # R expression(s)
                       filetype = NULL, # character vector
                                      # (valid) file formats
                       prefix = NULL, # char length 1
                                      # file prefix
                       path = NULL # char length 1
                                    # directory
                       ) {
                       
    # First split expression up by ";" in case multiple expressions are stacked
    # with each other
    if(is.list(expr))
        expr <- unlist(expr)
    expr <- unlist(strsplit(expr,split=";"))
        
    ##Test filetype is valid, prefix, path
    
    # Testing filetype is valid
    filetype<-tolower(filetype)
    if (testValidFiletypes())
        #change "ps" to "postscript"
    
    # Testing file prefix is valid
    #prefix must be length 1 OR length expr
    if (length(prefix) == 1) || length(prefix) == length(expr))
        prefix<-as.character(prefix) else
        stop("prefix must be length 1 or same as expr")
    
    ##Change this so it runs multiple paths?
    if (length(path) > 1) {
        warning("object ", sQuote("path"), " has more than one ",
                "element: only the first used in ", sQuote("path"), 
                call. = FALSE)
        path <- path[1]
    }
    
    
    
    
    numFiles <- length(expr)
    nameFormat <- paste(prefix,"-%0",nchar(numFiles),"d",sep="") 
#   filenames <- sprintf(nameFormat,prefix,1:numFiles)
    # result <- list of length(filetype)
    for(i in 1:length(filetype))
        #open filetype
        # or openGraphics(filetype[i])
        result <- sapply(expr, evalPlotCode) #result[i]
    #graphics.off()
    invisible(result)
}

"evalPlotCode" <- function(expr) {
    print("evalPlotCode called with:")
#      tryCatch(eval(parse(text = paste("Print(\"Tried to print:",expr,"\")"))), 
 #                        error = function(e) { 
  #                                   paste("Error in", expr, ":",
   #                                  geterrmessage())
    #                             }, 
     #                    warning = function(w) {
      #                                 paste("Warning in", expr, 
       #                                ":", conditionMessage(w))
        #                           })
}

#------------------------------
#
# testValidFiletypes()
#
#------------------------------
"testValidFiletypes" <- function(filetypes) {
    validFiletypes <- c("pdf", "png", "ps", "bmp")
    if (.Platform$OS.type != "windows")
        validFiletypes<-validFiletypes[-4]

    if(any(duplicated(filetypes))) {
        warning("duplicated filetypes!.. ...at? ..removed/ignored?")
    }  else if (all(filetypes %in% validFiletypes)) {
        return(TRUE)
    } else {
        warning("this filetype is not valid...")
        return(FALSE)
    }
}

