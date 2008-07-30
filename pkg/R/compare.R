# --------------------------------------------------------------------
# compare.R
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#
# compare()
#
# compare will take two(?) image files of the same format and identify
# any differences in the files.
# compare has the option of deleting the test file and the model file
# once the comparison has been made, and then to retain files if they
# are different.
#
# Note: Plots of diffs and .diff files are placed in the test directory
#
# --------------------------------------------------------------------
"compare" <- function(test,
                      control,
                      erase = c("none", "identical", "files", "all")
                      ##directory if empty?
                      #clean = c("full", "files", "none") # char length 1
                                    # none = don't delete anything
                                    # full = delete everything
                                    # files = delete files (but NOT index)
                                    # AFTER EACH expr
                                    ## diff plots?
                      ) {
    # Start warning handler
    assign("graphicsQCWarnings", character(0), envir = globalenv())
    if (is.character(test) && is.character(control) && test == control) {
        ## if they're in the same dir it's just asking for trouble
        # when trying to get QCResult..
        stop(sQuote("test"), " and ", sQuote("control"), " paths cannot be ",
             "the same")
    }
    if (length(erase) != 1) {
    	stop(sQuote("erase"), " must be one of ", dQuote("none"), ", ", 
    	     dQuote("files"), ", ", dQuote("identical"), ", or ",
    	     dQuote("all"))
    }

    test <- getQCResult(test)
    control <- getQCResult(control) 
    if (inherits(test, "list") || inherits(control, "list")) {
        if (inherits(test, "list") && inherits(control, "list")) {
            RESULT <- mapply(compare, test, list, erase) ## return?
            notYetImplemented()
        } else {
            stop("can't have a list of em vs. one") ##
        }
    }
    ## first package, then:
    if (inherits(test, "qcPlotFunResult") && inherits(control,
                                                         "qcPlotFunResult")) {
        results <- mapply(compareExpr, test, control, MoreArgs = 
                                          list(erase=erase), SIMPLIFY = FALSE)
    } else if (inherits(test, "qcPlotExprResult") && inherits(control,
                                                        "qcPlotExprResult")) {
        results <- compareExpr(test, control, erase)
    } else {
        ## test and control are not the same classes!
        notYetImplemented()
    }
    # Clear warning handler
    rm(graphicsQCWarnings, envir = globalenv())
    results
}

# -------------------------------------------------------------------- 
#
# getQCResult()
#
# --------------------------------------------------------------------
"getQCResult" <- function(result) {
    if (is.character(result)) {
        fileInfo <- file.info(result)
        if (is.na(fileInfo$isdir)) {
            stop("file ", dQuote(result), " not found", call. = FALSE)
        } else if (!fileInfo$isdir) {
            # It's a file
            return(readLog(result))
        } else {
            # It's a PATH (not a file) !
            # so autodetect log files

            ##first autodetect for packages
            if (length(files <- list.files(result, "-packageLog.xml",
                                           full.names = TRUE)) > 0) {
                if (length(files) == 1) {
                    return(readLog(files))
                } ## else it's many packageLog files..
            } else if (length(files <- list.files(result, "-funLog.xml",
                                                  full.names = TRUE)) > 0) {
                if (length(files) == 1) {
                    return(readLog(files))
                } ## else it's many funLog files..
            } else if (length(files <- list.files(result, "-log.xml",
                                                  full.names = TRUE)) > 0) {
                if (length(files) == 1) {
                    return(readPlotExprLog(files))
                } ##  Else it's many plotExprLog files so return the 
                  #list of them
                  #return(files)
            }
        }        
    } else if (inherits(result, c("qcPlotExprResult", "qcPlotFunResult",
                                                    "qcPlotPackageResult"))) {
        return(result)
    } else {
        stop(sQuote(result), "is not a graphicsQC result", call. = FALSE)
    }

}

# --------------------------------------------------------------------
#
# compareExpr()
#
# --------------------------------------------------------------------
"compareExpr" <- function(test, control, erase) {
    filePairs <- getPairs(test, control)
    # names(filepairs) are the filetypes to compare
    results <- lapply(names(filePairs[["test"]]), compareType, 
                 filePairs[["control"]], control[["info"]][["directory"]], 
                 filePairs[["test"]], test[["info"]][["directory"]], erase)    
    results[["unpaired"]] <- filePairs[["unpaired"]]
    results    
    # Compare one plotExpr at a time (using natural order of qcresult
    # OR order specified explicitly by log files OR the natural
    # order of the log files from the autodetect).
}

# --------------------------------------------------------------------
#
# getPairs()
#
# --------------------------------------------------------------------
"getPairs" <- function(test, control) {
    testFiletypes <- test[["info"]][["filetype"]]
    controlFiletypes <- test[["info"]][["filetype"]]
    filetypes <- unique(c(testFiletypes, controlFiletypes))
    testPairs <- vector("list", 0)
    controlPairs <- vector("list", 0)
    unpaired <- c()
    for (filetype in filetypes) {
        testPairs[filetype] <- list(grep(filetype, test[["filenames"]],
                                         value = TRUE))
        controlPairs[filetype] <- list(grep(filetype, control[["filenames"]],
                                            value = TRUE))
        testPairsLength <- length(testPairs[[filetype]])
        controlPairsLength <- length(controlPairs[[filetype]])
        if (testPairsLength != controlPairsLength) {
            warningHandler("length of files to compare are different;",
                    " unpaired files ignored")
            # If the amount of files for a given filetype have different
            # length, put the leftovers in 'unpaired' and cut the group
            # with more files down to size
            # NB: when this is the case, it is likely that one extra plot in
            # the middle of the other plots would cause the rest to fail.
            if (testPairsLength > controlPairsLength) {
                unpaired <- c(unpaired, testPairs[[filetype]][(
                              controlPairsLength + 1):testPairsLength])
                if (controlPairsLength == 0) {
                    testPairs[[filetype]] <- NULL
                } else {
                    testPairs[[filetype]] <- testPairs[[filetype]][
                                                        1:controlPairsLength]
                }
            } else {
                unpaired <- c(unpaired, controlPairs[[filetype]][(
                              testPairsLength + 1):controlPairsLength])
                if (testPairsLength == 0) {
                    controlPairs[[filetype]] <- NULL
                } else {
                    controlPairs[[filetype]] <- controlPairs[[filetype]][
                                                           1:testPairsLength]
                }
            }
        }
    }
    return(list("control" = controlPairs, "test" = testPairs,
                "unpaired" = unpaired))
}

## Note: To extend this to allow for more filetypes, just add a function
## "compareNEWTYPE". Once it has been added to valid types in makeplots, that
## should be all the changes required here.
# --------------------------------------------------------------------
#
# compareType()
#
# --------------------------------------------------------------------
"compareType" <- function(filetype, control, controlPath, test, testPath,
                          erase) {
    # pastes "compare" and 'filetype' to call the appropriate function;
    # pastes 'path' and 'filename' for control and test groups respectively;
    # passes IM capability if it's supported and a diff plot is required
    mapply(paste("compare", toupper(filetype), sep = ""), 
           paste(controlPath, .Platform$file.sep,
           control[[filetype]], sep = ""), paste(testPath, .Platform$file.sep,
           test[[filetype]], sep = ""), hasIM() &&
           filetype %in% getSupportedIMFormats() && 
           any(erase == c("none", "identical")), testPath) ##rather than test
           # path, just setwd in case original path was "testdir" rather than
           # "testdir/"
    ## currently path is test dir
    ## now remove files?

}

# --------------------------------------------------------------------
#
# comparePDF()
#
# --------------------------------------------------------------------
"comparePDF" <- function(file1, file2, useIM, diffPlotPath) {
    ## Just compare and ignore the first 6 lines (creationdate/moddate)?
    ## or take previous approach and re-write file without header and xref
    diffName <- getDiffName(file1, file2)
    diffResult <- GNUdiff(file1, file2, paste(diffPlotPath,
            .Platform$file.sep, diffName, ".diff", sep = ""))
    ##diffResult (the actual file) might not even exist.. didn't stop()
    if (diffResult != "identical" && length(readLines(diffResult, n = 7)) 
                                                                        > 6) {
        # There is a true difference, not just the dates/times
        if (useIM) {
            makeIMDiffPlot(file1, file2, paste(diffPlotPath, 
                              .Platform$file.sep, diffName, ".png", sep = ""))
        }
        return(diffResult)
    } else {
        # Files are the same or just the dates/times were different
        file.remove(diffResult)
        return("identical")
    }
}

# --------------------------------------------------------------------
#
# comparePS()
#
# --------------------------------------------------------------------
"comparePS" <- function(file1, file2, useIM, diffPlotPath) {
    diffName <- getDiffName(file1, file2)
    diffResult <- GNUdiff(file1, file2, paste(diffPlotPath,
            .Platform$file.sep, diffName, ".diff", sep = ""))
    if (useIM && diffResult != "identical") {
        makeIMDiffPlot(file1, file2, paste(diffPlotPath,
                              .Platform$file.sep, diffName, ".png", sep = ""))
    }
    return(diffResult)
}

# --------------------------------------------------------------------
#
# comparePNG()
#
# --------------------------------------------------------------------
"comparePNG" <- function(file1, file2, useIM, diffPlotPath) {
    diffName <- getDiffName(file1, file2)
    diffResult <- GNUdiff(file1, file2, paste(diffPlotPath,
            .Platform$file.sep, diffName, ".diff", sep = ""))
    if (useIM && diffResult != "identical") {
        makeIMDiffPlot(file1, file2, paste(diffPlotPath,
                 .Platform$file.sep, diffName, ".png", sep = ""))
    }
    return(diffResult)
}

# --------------------------------------------------------------------
#
# compareBMP()
#
# --------------------------------------------------------------------
"compareBMP" <- function(file1, file2, useIM, diffPlotPath) {
    diffName <- getDiffName(file1, file2)
    diffResult <- GNUdiff(file1, file2, paste(diffPlotPath,
            .Platform$file.sep, diffName, ".diff", sep = ""))
    if (useIM && diffResult != "identical") {
        makeIMDiffPlot(file1, file2, paste(diffPlotPath,
                 .Platform$file.sep, diffName, ".png", sep = ""))
    }
    return(diffResult)
}

# --------------------------------------------------------------------
#
# getDiffName()
#
# --------------------------------------------------------------------
"getDiffName" <- function(file1, file2) {
    set1 <- unlist(strsplit(file1, .Platform$file.sep))
    set1 <- set1[length(set1)]
    set2 <- unlist(strsplit(file2, .Platform$file.sep))
    set2 <- set2[length(set2)]
    paste(gsub("[.]", "-", set1), "+",  gsub("[.]", "-", set2), sep = "")
}

# --------------------------------------------------------------------
#
# GNUdiff()
#
# --------------------------------------------------------------------
"GNUdiff" <- function(file1, file2, outDiffFile = "") {
#                               diffArgs = "-q", intern = FALSE) {
    ## *nix only? system() + exit status
    ## This requires a bit more work for windows support
    diffResult <- system(paste("diff", file1, file2, ">", outDiffFile),
                         ignore.stderr = TRUE)
    if (diffResult == 0) {
        # Delete empty diff file
        file.remove(outDiffFile)
        return("identical")
    } else if (diffResult == 256) {
        return(outDiffFile)
    } else {
        # If one of the files doesn't exist, the .diff file will be empty
        if (!file.exists(file1)) {
            warning("file ", file1, " not found; marked as different")
        }
        if (!file.exists(file2)) {
            warning("file ", file2, " not found; marked as different")
        }
        return(outDiffFile) ##return("error")?
    }
    
}

# --------------------------------------------------------------------
#
# makeIMDiffPlot()
#
# --------------------------------------------------------------------
"makeIMDiffPlot" <- function(file1, file2, newFilename) {
    system(paste("compare", file1, file2, newFilename))
}

# --------------------------------------------------------------------
#
# hasDiff()
#
# --------------------------------------------------------------------
"hasDiff" <- function() {
    length(grep("GNU diffutils", try(system("diff -v",
                                            intern = TRUE)[1]))) > 0
}

# --------------------------------------------------------------------
#
# hasIM()
#
# --------------------------------------------------------------------
"hasIM" <- function() {
    length(grep("ImageMagick", try(system("compare -version",
                                          intern = TRUE)[1]))) > 0
}

# --------------------------------------------------------------------
#
# getSupportedIMFormats()
#
# --------------------------------------------------------------------
"getSupportedIMFormats" <- function() {
    supportedFormats <- character(0)
    formats <- system("identify -list Format", intern = TRUE,
                      ignore.stderr = TRUE) ##this command has trouble..
    bmpLine <- grep("Microsoft Windows bitmap image$", formats, value = TRUE)
    pdfLine <- grep("Portable Document Format$", formats, value = TRUE)
    pngLine <- grep("Portable Network Graphics", formats, value = TRUE)
    psLine <- grep("PostScript$", formats, value = TRUE)
    if (length(bmpLine > 0) && grep("[ ]r[^ ][^ ][ ]", bmpLine) > 0) {
        supportedFormats <- c(supportedFormats, "bmp")
    }
    if (length(pdfLine > 0) && grep("[ ]r[^ ][^ ][ ]", pdfLine) > 0) {
        supportedFormats <- c(supportedFormats, "pdf")
    }
    if (length(pngLine > 0) && grep("[ ]rw[^ ][ ]", pngLine) > 0) {
        supportedFormats <- c(supportedFormats, "png")
    }
    if (length(psLine > 0) && grep("[ ]r[^ ][^ ][ ]", psLine) > 0) {
        supportedFormats <- c(supportedFormats, "ps")
    }
    return(supportedFormats)
}

# --------------------------------------------------------------------
#
# readPlotExprLog()
#
# --------------------------------------------------------------------
"readPlotExprLog" <- function(filename) {
## better error handling on bad files?
    readLogHandler = function() {
        qcResult <- list(filenames = character(0), warnings = NULL,
                         errors = NULL)
        qcInfo <- list()
        list("filename" = function(x) {
             ## anything more efficient than c(value, x) ?
             ## xmlSApply more costly? see ?xmlTreeParse
             ## assign() slightly neater.. but less efficient?
             qcResult[["filenames"]] <<- c(qcResult[["filenames"]],
                                           xmlValue(x[[1]]))
             },
             "warning" = function(x) {
                 qcResult[["warnings"]] <<- c(qcResult[["warnings"]],
                                              xmlValue(x[[1]]))
             },
             "error" = function(x) {
                 qcResult[["errors"]] <<- c(qcResult[["errors"]],
                                            xmlValue(x[[1]]))
             },
             "OS" = function(x) { 
                 qcInfo[["OS"]] <<- c(qcInfo[["OS"]], xmlValue(x[[1]])) 
             },
             "Rver" = function(x) { 
                 qcInfo["Rver"] <<- c(qcInfo[["Rver"]], xmlValue(x[[1]]))
             },
             "date" = function(x) { 
                 qcInfo["date"] <<- c(qcInfo[["date"]], xmlValue(x[[1]])) 
                             },
             "call" = function(x) { 
                 qcInfo["call"] <<- c(qcInfo[["call"]],
                                                         xmlValue(x[[1]]))
             },
             "filetype" = function(x) {
                 qcInfo[["filetype"]] <<- c(qcInfo[["filetype"]],
                                            xmlValue(x[[1]]))
             },
             "directory" = function(x) {
                 qcInfo[["directory"]] <<- c(qcInfo[["directory"]],
                                            xmlValue(x[[1]]))
             },
             "getResult" = function() {
                 qcResult[["info"]] <<- qcInfo
                 return(qcResult)
             }
             )
    }    
    h = readLogHandler()    
    xmlTreeParse(filename, handlers = h) #useInternalNodes = TRUE) (call fails) 
    logQCResult <- h$getResult()
    class(logQCResult) <- "qcPlotExprResult"
    return(logQCResult)
}

# -------------------------------------------------------------------- 
#
# getLogType()
#
# --------------------------------------------------------------------
"getLogType" <- function(result) {
    validLogTypes <- c("qcPlotExprResult", "qcPlotFunResult",
                       "qcPlotPackageResult")
    if (length(grep("[Ll]og[.]xml$", result)) > 0) {
        type <- xmlName(xmlRoot(xmlTreeParse(result)))
        if (type %in% validLogTypes) {
            return(type)
        }
    } 
    stop("file given for test/control must be a qc log file")
}

# -------------------------------------------------------------------- 
#
# readLog()
#
# --------------------------------------------------------------------
"readLog" <- function(logFile) {
    library(XML) ## might fail
    logType <- getLogType(logFile)
    if (logType == "qcPlotExprResult") {
        return(readPlotExprLog(logFile))
    } else if (logType == "qcPlotFunResult") {
        return(readPlotFunLog(logFile))
    } ## else plotPackageResult
}

# -------------------------------------------------------------------- 
#
# readPlotFunLog()
#
# --------------------------------------------------------------------
"readPlotFunLog" <- function(logFile) {
    exprResults <- unlist(lapply(xmlChildren(xmlRoot(xmlTreeParse(logFile)
                                                               )), xmlValue))
    names(exprResults) <- NULL
    funResults <- lapply(exprResults, readPlotExprLog)
    class(funResults) <- "qcPlotFunResult"
    funResults
}

# -------------------------------------------------------------------- 
#
# warningHandler()
#
# --------------------------------------------------------------------
"warningHandler" <- function(...) {
    stringWarning <- paste(..., sep = "")
    # Only show warnings we haven't seen before
    if (!stringWarning %in% graphicsQCWarnings) {
        assign("graphicsQCWarnings", c(graphicsQCWarnings, stringWarning),
                                                          envir = globalenv())
        warning(stringWarning, call. = FALSE)
    }
}

