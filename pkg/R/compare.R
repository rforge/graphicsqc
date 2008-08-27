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
    # Clear warning handler on exit
    on.exit(rm(graphicsQCWarnings, envir = globalenv()))
    
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
            notYetImplemented()
            RESULT <- mapply(compare, test, list, erase) ## return?
        } else {
            stop("can't have a list of em vs. one") ##
            notYetImplemented()
        }
    }
    ## first package, then:
    if (inherits(test, "qcPlotFunResult") &&
        inherits(control, "qcPlotFunResult")) {
        results <- mapply(compareExpr, test, control,
                          MoreArgs = list(erase=erase), SIMPLIFY = FALSE)
        writeXmlCompareTypeLog(results, "Fun")
        class(results) <- "qcCompareFunResult"
        return(results)
    } else if (inherits(test, "qcPlotFileResult") &&
        inherits(control, "qcPlotFileResult")) {
        results <- mapply(compareExpr, test, control,
                          MoreArgs = list(erase=erase), SIMPLIFY = FALSE)
        writeXmlCompareTypeLog(results, "File")
        class(results) <- "qcCompareFileResult"
        return(results)
    } else if (inherits(test, "qcPlotExprResult") &&
               inherits(control, "qcPlotExprResult")) {
        results <- compareExpr(test, control, erase)
        return(results)
    } else {
        ## test and control are not the same classes!
        notYetImplemented()
    }
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
            } else if (length(files <- list.files(result, "-fileLog.xml",
                                                  full.names = TRUE)) > 0) {
                if (length(files) == 1) {
                    return(readLog(files))
                } ## else it's many fileLog files..
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
                        "qcPlotFileResult", "qcPlotPackageResult"))) {
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
    if (length(filePairs[[1]]) < length(filePairs[[2]])) {
        names(results) <- names(filePairs[[1]])
    } else {
        names(results) <- names(filePairs[[2]])
    }
    info <- list("OS" = .Platform$OS.type, "Rver" = 
                 as.character(getRversion()), "date" = date(),
                 "call" = paste(deparse(sys.call(1)), collapse = ""),
                 "controlDirectory" = control[["info"]][["directory"]],
                 "testDirectory" = test[["info"]][["directory"]],
                 "logFilename" = getCompareExprLogFilename(test, control))
    results[["unpaired"]] <- filePairs[["unpaired"]]
    results <- list("info" = info, "testInfo" = test[["info"]],
                    "controlInfo" = control[["info"]], "results" = results)
    class(results) <- "qcCompareExprResult"
    writeXmlCompareExprLog(results)
    results
    # Compare one plotExpr at a time (using natural order of qcresult
    # OR order specified explicitly by log files OR the natural
    # order of the log files from the autodetect).
}

# --------------------------------------------------------------------
#
# getPairs()
#
# Also sorts unpaired warnings
# --------------------------------------------------------------------
"getPairs" <- function(test, control) {
    testFiletypes <- names(test[["plots"]])
    controlFiletypes <- names(control[["plots"]])

    # If the amount of files for a given filetype have different
    # length, put the leftovers in 'unpaired' and cut the group
    # with more files down to size
    # NB: when this is the case, it is likely that one extra plot in
    # the middle of the other plots would cause the rest to fail.

    allFiletypes <- unique(c(testFiletypes, controlFiletypes))
    filetypesToCompare <- controlFiletypes[controlFiletypes %in% testFiletypes]
    
    controlUnpairedFiletype <- controlFiletypes[!(controlFiletypes %in%
                                                  filetypesToCompare)]
    testUnpairedFiletype <- testFiletypes[!(testFiletypes %in%
                                            filetypesToCompare)]
    
    # Control filetypes are being compared, everything else is unpaired
    controlPaired <- vector("list", length(controlFiletypes))
    names(controlPaired) <- controlFiletypes
    testPaired <- controlPaired
    
    testUnpaired <- vector("list", length(testFiletypes))
    names(testUnpaired) <- testFiletypes
    controlUnpaired <- controlPaired
    # First take out all the ones that aren't even paired filetypes
    lapply(controlUnpairedFiletype, function(type) {
        lapply(names(control[["plots"]][[type]]), function (ele)
            if (!is.null(control[["plots"]][[type]][[ele]])) {
                controlUnpaired[[type]][ele] <<- list(control[["plots"]][[
                                                               type]][[ele]])
            })
        if (length(controlUnpaired[[type]]) == 0) controlUnpaired[[type]] <<- NULL
        })
    lapply(testUnpairedFiletype, function(type) {
        lapply(names(test[["plots"]][[type]]), function (ele)
            if (!is.null(test[["plots"]][[type]][[ele]])) {
                testUnpaired[[type]][ele] <<- list(test[["plots"]][[
                                                         type]][[ele]])
            })
        if (length(controlUnpaired[[type]]) == 0) testUnpaired[[type]] <<- NULL
        })
    # Take out unpaired files within the paired filetypes
    lapply(filetypesToCompare, function (filetype) {
        testUnpairedOutput <- controlUnpairedOutput <-
                                    list(warnings=NULL, errors=NULL, plot=NULL)
        for (warns in c("warnings", "errors")) {
            testWarns <- test[["plots"]][[filetype]][[warns]]
            controlWarns <- control[["plots"]][[filetype]][[warns]]
            unpTestWarns <- testWarns[!(testWarns %in% controlWarns)]
            testUnpairedOutput[[warns]] <- unpTestWarns
            unpControlWarns <- controlWarns[!(controlWarns %in% testWarns)]
            controlUnpairedOutput[[warns]] <- unpControlWarns
        }

        testPlotIndices <- seq_along(test[["plots"]][[filetype]][["plot"]])
        controlPlotIndices <- seq_along(control[["plots"]][[
                                                          filetype]][["plot"]])
        shortest <- seq_len(min(length(testPlotIndices),
                                length(controlPlotIndices)))
        unpTestPlots <- test[["plots"]][[filetype]][["plot"]][
                                              !(testPlotIndices %in% shortest)]
        if (length(unpTestPlots) == 0) unpTestPlots <- NULL
        testUnpairedOutput[["plot"]] <- unpTestPlots
        unpControlPlots <- control[["plots"]][[filetype]][["plot"]][
                                           !(controlPlotIndices %in% shortest)]
        if (length(unpControlPlots) == 0) unpControlPlots <- NULL
        controlUnpairedOutput[["plot"]] <- unpControlPlots
        if (length(unpTestPlots) > 0 || length(unpControlPlots) > 0) {
            warningHandler("length of files to compare are different;",
                           " unpaired files ignored")
        }
        
        if(length(testUnpairedOutput) == 0) testUnpairedOutput <- NULL
        testUnpaired[[filetype]] <<- testUnpairedOutput
        if(length(controlUnpairedOutput) == 0) controlUnpairedOutput <- NULL
        
        controlUnpaired[[filetype]] <<- controlUnpairedOutput
        
        controlPaired[filetype] <<- list(control[["plots"]][[filetype]][[
                                                         "plot"]][shortest])
        testPaired[filetype] <<- list(test[["plots"]][[filetype]][[
                                                         "plot"]][shortest])
        })
    # If the unpaireds are just list() or blank, change them to NULL
    # for consistency when reading the XML file later on
    if (length(testUnpaired) == 0) testUnpaired <- NULL
    if (length(controlUnpaired) == 0) controlUnpaired <- NULL
    return(list("test" = testPaired, "control" = controlPaired, "unpaired" = 
         list("test"=testUnpaired, "control"=controlUnpaired)))
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
    if (length(test[[filetype]]) == 0) {
        return(NULL)
    } else {
        result <- mapply(paste("compare", toupper(filetype), sep = ""), 
           paste(testPath, test[[filetype]], sep = .Platform$file.sep),
           paste(controlPath, control[[filetype]], sep = .Platform$file.sep),
           hasIM() && filetype %in% getSupportedIMFormats() && 
           any(erase == c("none", "identical")), testPath, SIMPLIFY = FALSE)
        names(result) <- NULL
        return(result)
           ##rather than test path, just setwd in case original path was
           # "testdir" rather than "testdir/"
    }
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
    diffFileName <- paste(diffName, ".diff", sep = "")
    diffPlotName <- paste(diffName, ".png", sep = "")
    diffFilePath <- paste(diffPlotPath, diffFileName, sep = .Platform$file.sep)
    diffResult <- GNUdiff(file1, file2, diffFilePath)
    ##diffResult (the actual file) might not even exist.. didn't stop()
    if (diffResult == "different" && length(readLines(diffFilePath, n = 7)) 
                                                                        > 6) {
        # There is a true difference, not just the dates/times
        if (useIM) {
            diffPlot <- paste(diffPlotPath, diffPlotName,
                              sep = .Platform$file.sep)
            makeIMDiffPlot(file1, file2, diffPlot)
        }
    } else {
        # Files are the same or just the dates/times were different
        file.remove(diffFilePath)
        diffFileName <- diffPlotName <- NULL
        diffResult <- "identical"
    }
    return(list(controlFile=file2, testFile=file1, result=diffResult,
                                diffFile=diffFileName, diffPlot=diffPlotName))
}

# --------------------------------------------------------------------
#
# comparePS()
#
# --------------------------------------------------------------------
"comparePS" <- function(file1, file2, useIM, diffPlotPath) {
    diffName <- getDiffName(file1, file2)
    diffFileName <- paste(diffName, ".diff", sep = "")
    diffPlotName <- paste(diffName, ".png", sep = "")
    diffFilePath <- paste(diffPlotPath, diffFileName, sep = .Platform$file.sep)
    diffResult <- GNUdiff(file1, file2, diffFilePath)
    if (diffResult == "different") {
        if (useIM) {
            diffPlot <- paste(diffPlotPath, diffPlotName,
                              sep = .Platform$file.sep)
            makeIMDiffPlot(file1, file2, diffPlot)
        }
    } else {
        diffFileName <- diffPlotName <- NULL
    }
    return(list(controlFile=file2, testFile=file1, result=diffResult,
                                diffFile=diffFileName, diffPlot=diffPlotName))
}

# --------------------------------------------------------------------
#
# comparePNG()
#
# --------------------------------------------------------------------
"comparePNG" <- function(file1, file2, useIM, diffPlotPath) {
    diffName <- getDiffName(file1, file2)
    diffPlotName <- NULL
    diffResult <- GNUdiff(file1, file2)
    if (useIM && diffResult == "different") {
        diffPlotName <- paste(diffName, ".png", sep = "")
        diffPlot <- paste(diffPlotPath, diffPlotName,
                          sep = .Platform$file.sep)
        makeIMDiffPlot(file1, file2, diffPlot)
    }
    return(list(controlFile=file2, testFile=file1, result=diffResult,
                                diffFile=NULL, diffPlot=diffPlotName))
}

# --------------------------------------------------------------------
#
# compareBMP()
#
# --------------------------------------------------------------------
"compareBMP" <- function(file1, file2, useIM, diffPlotPath) {
    diffName <- getDiffName(file1, file2)
    diffPlotName <- paste(diffName, ".png", sep = "")
    diffResult <- GNUdiff(file1, file2)
    if (useIM && diffResult == "different") {
        diffPlot <- paste(diffPlotPath, diffPlotName, sep = .Platform$file.sep)
        makeIMDiffPlot(file1, file2, diffPlot)
    }
    return(list(controlFile=file2, testFile=file1, result=diffResult,
                                diffFile=NULL, diffPlot=diffPlotName))
}

# --------------------------------------------------------------------
#
# getDiffName()
#
# --------------------------------------------------------------------
"getDiffName" <- function(file1, file2) {
    set1 <- basename(file1)
    set2 <- basename(file2)
    paste(gsub("[.]", "-", set1), "+",  gsub("[.]", "-", set2), sep = "")
}

# --------------------------------------------------------------------
#
# GNUdiff()
#
# --------------------------------------------------------------------
"GNUdiff" <- function(file1, file2, outDiffFile = NULL) {
                                            #diffArgs = "-q", intern = FALSE)
    ## *nix only? system() + exit status
    ## This requires a bit more work for windows support
    redirectOutput <- ""
    if (!is.null(outDiffFile)) {
        redirectOutput = paste(">", outDiffFile)
    }
    diffResult <- system(paste("diff", file1, file2, redirectOutput),
                         ignore.stderr = TRUE)
    if (diffResult == 0) {
        # Delete empty diff file
        if (!is.null(outDiffFile)) {
            file.remove(outDiffFile)
        }
        return("identical")
    } else {
        # If one of the files doesn't exist, the .diff file will be empty
        if (!file.exists(file1)) {
            warning("file ", file1, " not found; marked as different")
            if (!is.null(outDiffFile)) {
                file.remove(outDiffFile)
            }
        }
        if (!file.exists(file2)) {
            warning("file ", file2, " not found; marked as different")
            if (!is.null(outDiffFile)) {
                file.remove(outDiffFile)
            }
        }
        return("different")
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
    bmpLine <- grep("Microsoft Windows bitmap image$", formats, value = TRUE,
                    perl = TRUE, useBytes = TRUE)
    pdfLine <- grep("Portable Document Format$", formats, value = TRUE,
                    perl = TRUE, useBytes = TRUE)
    pngLine <- grep("Portable Network Graphics", formats, value = TRUE,
                    perl = TRUE, useBytes = TRUE)
    psLine <- grep("PostScript$", formats, value = TRUE, perl = TRUE,
                   useBytes = TRUE)
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
    logTree <- xmlRoot(xmlTreeParse(filename))
    
    # Read Info
    info <- xmlApply(logTree[[1]], xmlValue)
    
    # Read plot information
    plots <- lapply(seq(2, length(logTree)), function(i) 
                          mergeList(xmlApply(logTree[[i]], xmlValue)))
    names(plots) <- unlist(xmlApply(logTree, xmlAttrs))
    
    qcLogResult <- list(info = info, plots = plots)
    class(qcLogResult) <- "qcPlotExprResult"
    return(qcLogResult)
}

# -------------------------------------------------------------------- 
#
# mergeList()
# 
# Merges elements in named lists which have repeated names
# --------------------------------------------------------------------
mergeList <- function(x) {
    if (any(duplicated(names(x)))) {
        tags <- unique(names(x))
        output <- lapply(tags, function (tag)
                                   as.vector(unlist(x[names(x) == tag])))
        names(output) <- tags
        return(output)
    } else {
        return(x)
    }
}

# -------------------------------------------------------------------- 
#
# getLogType()
#
# --------------------------------------------------------------------
"getLogType" <- function(result) {
    validLogTypes <- c("qcPlotExprResult", "qcPlotFunResult",
                       "qcPlotFileResult", "qcPlotPackageResult",
                       "qcCompareExprResult", "qcCompareFunResult",
                       "qcCompareFileResult")
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
        return(readPlotFunLog(logFile, "qcPlotFunResult"))
    } else if (logType == "qcPlotFileResult") {
        return(readPlotFunLog(logFile, "qcPlotFileResult"))
    } else if (logType == "qcCompareExprResult") {
        return(readCompareExprLog(logFile))
    } else if (logType == "qcCompareFunResult") {
        return(readCompareFunLog(logFile, "qcCompareFunResult"))
    } else if (logType == "qcCompareFileResult") {
        return(readCompareFunLog(logFile, "qcCompareFileResult"))
    } ## else plotPackageResult
    notYetImplemented()
}

# -------------------------------------------------------------------- 
#
# readPlotFunLog()
#
# --------------------------------------------------------------------
"readPlotFunLog" <- function(logFile, logClass) {
    exprResults <- unlist(lapply(xmlChildren(xmlRoot(xmlTreeParse(logFile))),
                                 xmlValue))
    names(exprResults) <- NULL
    funResults <- lapply(exprResults, readPlotExprLog)
    class(funResults) <- logClass
    funResults
}

# --------------------------------------------------------------------
#
# writeXmlCompareExprLog()
#
# --------------------------------------------------------------------
"writeXmlCompareExprLog" <- function(results) {
    library(XML) ## might not run
    xmlResults <- xmlOutputDOM(tag="qcCompareExprResult")
    
    # Write info
    writeXmlInfo(xmlResults, results)
    writeXmlInfo(xmlResults, results, "testInfo")
    writeXmlInfo(xmlResults, results, "controlInfo")
    
    # Write comparisons
    resultNames <- names(results[["results"]])
    lapply(resultNames[resultNames != "unpaired"],
        function(type) {
            xmlResults$addTag("compare", close = FALSE, attrs = c(type=type))
             lapply(results[["results"]][[type]],
                 function(comparison) {
                     xmlResults$addTag("comparison", close = FALSE, attrs =
                                       c(controlFile = comparison$controlFile,
                                         testFile = comparison$testFile))
                      xmlResults$addTag("result", comparison[["result"]])
                      xmlResults$addTag("diffFile", comparison[["diffFile"]])
                      xmlResults$addTag("diffPlot", comparison[["diffPlot"]])
                     xmlResults$closeTag() # comparison
                 })
            xmlResults$closeTag() # compare
        })

    # Write unpaired
    xmlResults$addTag("unpaired", close = FALSE)
    lapply(c("test", "control"), # or names(results[["results"]][["unpaired"]]
        function(testOrControl) {
            xmlResults$addTag(testOrControl, close = FALSE)
            lapply(names(results[["results"]][["unpaired"]][[testOrControl]]),
                function(type) {
                    xmlResults$addTag(type, close = FALSE)
                    lapply(names(results[["results"]][["unpaired"]][[
                                          testOrControl]][[type]]),
                          function(ele) {
                              if (is.null(results[["results"]][["unpaired"]][[
                                          testOrControl]][[type]][[ele]])) {
                                  xmlResults$addTag(ele)
                              } else {
                                  lapply(results[["results"]][["unpaired"]][[
                                     testOrControl]][[type]][[ele]],
                                     xmlResults$addTag, tag=ele)
                              }
                          })
                   xmlResults$closeTag() # type
                })
            xmlResults$closeTag() # testOrControl
        })
    xmlResults$closeTag() # unpaired
    saveXML(xmlResults, paste(results[["info"]][["testDirectory"]],
                              results[["info"]][["logFilename"]],
                              sep = .Platform$file.sep))
}

# --------------------------------------------------------------------
#
# writeXmlCompareTypeLog()
#
# --------------------------------------------------------------------
"writeXmlCompareTypeLog" <- function(results, type) {
    library(XML) ## might not run

    xmlResults <- xmlOutputDOM(tag=paste("qcCompare", type, "Result",
                                         sep = ""))
     logs <- sapply(seq_along(results), function(i)
                 paste(results[[i]][["testInfo"]][["directory"]],
                       results[[i]][["info"]][["logFilename"]],
                       sep = .Platform$file.sep))
     lapply(logs, xmlResults$addTag, tag="qcCompareExprResult")
     filename <- paste(unlist(strsplit(results[[1]][["testInfo"]][[
                       "logFilename"]], "-log.xml")), "-compare", type,
                       "Log.xml", sep = "")
    saveXML(xmlResults, paste(results[[1]][["testInfo"]][["directory"]],
                              filename, sep = .Platform$file.sep))
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

# -------------------------------------------------------------------- 
#
# getCompareExprLogFilename()
#
# --------------------------------------------------------------------
"getCompareExprLogFilename" <- function(test, control) {
    testPrefix <- unlist(strsplit(test[["info"]][["logFilename"]],
                         "-log.xml"))
    controlPrefix <- unlist(strsplit(control[["info"]][["logFilename"]],
                            "-log.xml"))
    paste(testPrefix, "+", controlPrefix, "-compareExprLog.xml", sep = "")
}

