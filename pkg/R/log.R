# --------------------------------------------------------------------
# log.R
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#
# writeXmlPlotExprLog()
#
# --------------------------------------------------------------------
"writeXmlPlotExprLog" <- function(results) {
    xmlResults <- xmlOutputDOM(tag="qcPlotExprResult")

    # Add info to XML
    writeXmlInfo(xmlResults, results)

    # Write plots for each filetype including warnings/error
    lapply(names(results[["plots"]]),
        function(type) {
            xmlResults$addTag("plots", close = FALSE, attrs=c(type=type))
             lapply(names(results[["plots"]][[type]]),
                 function(x) {
                     if (is.null(results[["plots"]][[type]][[x]])) {
                         xmlResults$addTag(x)
                     } else {
                         lapply(results[["plots"]][[type]][[x]],
                                xmlResults$addTag, tag=x)
                     }
                 })
            xmlResults$closeTag() # plots
        })
    saveXML(xmlResults, results[["info"]][["logFilename"]])
}

# --------------------------------------------------------------------
#
# writeXmlInfo()
#
# --------------------------------------------------------------------
"writeXmlInfo" <- function(xmlResults, results, tag="info") {
    xmlResults$addTag(tag, close = FALSE)
     mapply(function(name, info) {
                if (name == "call") {
                    xmlResults$addTag("call", close = FALSE)
                     xmlResults$addCData(info)
                    xmlResults$closeTag() # call
                } else {
                    xmlResults$addTag(name, info)
                }
            }
     , names(results[[tag]]), results[[tag]])
    xmlResults$closeTag() # info
}

# --------------------------------------------------------------------
#
# writeXmlPlotTypeLog()
#
# --------------------------------------------------------------------
"writeXmlPlotTypeLog" <- function(exprPrefix, path, filePrefix, type) {
    xmlResults <- xmlOutputDOM(tag=paste("qcPlot", chartr("f", "F", type),
                                         "Result", sep = ""))
     lapply(paste(path, exprPrefix, "-log.xml", sep = ""), xmlResults$addTag,
            tag="qcPlotExprResult")
    saveXML(xmlResults, paste(path, filePrefix, "-", type, "Log.xml",
                              sep = ""))
}

# --------------------------------------------------------------------
#
# writeXmlCompareExprLog()
#
# --------------------------------------------------------------------
"writeXmlCompareExprLog" <- function(results) {
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
             comparisons <- if (is.null(names(results[["results"]][[type]]))) {
                                seq_along(results[["results"]][[type]])
                            } else {
                                names(results[["results"]][[type]]) == ""
                            }
             lapply(results[["results"]][[type]][comparisons],
                 function(comparison) {
                     xmlResults$addTag("comparison", close = FALSE, attrs =
                                       c(controlFile = comparison$controlFile,
                                         testFile = comparison$testFile))
                      xmlResults$addTag("result", comparison[["result"]])
                      xmlResults$addTag("diffFile", comparison[["diffFile"]])
                      xmlResults$addTag("diffPlot", comparison[["diffPlot"]])
                     xmlResults$closeTag() # comparison
                 })
             lapply(names(results[["results"]][[type]][!comparisons]),
                  function(warnsOrError) {
                      lapply(results[["results"]][[type]][!comparisons][[
                                      warnsOrError]],
                             xmlResults$addTag, tag=warnsOrError)
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
    saveXML(xmlResults, paste(results[["info"]][["path"]],
                              results[["info"]][["logFilename"]],
                              sep = .Platform$file.sep))
}

# --------------------------------------------------------------------
#
# writeXmlCompareTypeLog()
#
# --------------------------------------------------------------------
"writeXmlCompareTypeLog" <- function(results, type, filename) {
    xmlResults <- xmlOutputDOM(tag = paste("qcCompare", type, "Result",
                               sep = ""),
                               attrs = c(path = attr(results, "path")))
     logs <- sapply(seq_along(results), function(i)
                 paste(results[[i]][["info"]][["path"]],
                       results[[i]][["info"]][["logFilename"]],
                       sep = .Platform$file.sep))
     lapply(logs, xmlResults$addTag, tag="qcCompareExprResult")
    saveXML(xmlResults, paste(results[[1]][["info"]][["path"]],
                              filename, sep = .Platform$file.sep))
}

# --------------------------------------------------------------------
#
# readLog()
#
# Takes a character vector specifying the location of the log file to read in.
# --------------------------------------------------------------------
"readLog" <- function(logFile) {
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
# readCompareExprLog()
#
# --------------------------------------------------------------------
"readCompareExprLog" <- function(filename) {
    ## better error handling on bad files?
    # Gets the overall tree, then collects sub-parts, then combines them
    comparisonTree <- xmlRoot(xmlTreeParse(filename))
    info <- xmlApply(comparisonTree[[1]], xmlValue)
    testInfo <- mergeList(xmlApply(comparisonTree[[2]], xmlValue))
    controlInfo <- mergeList(xmlApply(comparisonTree[[3]], xmlValue))

    topLevelElements <- xmlApply(comparisonTree, xmlAttrs)

    # Get results for each filetype
    types <- unlist(topLevelElements[which(names(topLevelElements) ==
                    "compare")])
    filetypeResults <- mapply(function(type, i) {
        controlAndTest <- lapply(xmlApply(comparisonTree[[i]], xmlAttrs),
                                 as.list, all.names = TRUE)
        warnsAndErrorIndices <- names(controlAndTest) != "comparison"
        controlAndTest <- controlAndTest[!warnsAndErrorIndices]

        resultDiffPlot <- xmlApply(comparisonTree[[i]], function(tree) {
                                   xmlApply(tree, xmlValue) })
        warnsAndError <- mergeList(resultDiffPlot[warnsAndErrorIndices])
        resultDiffPlot <- resultDiffPlot[!warnsAndErrorIndices]

        combined <- lapply(seq_len(length(controlAndTest)), function(j)
                           c(controlAndTest[[j]], resultDiffPlot[[j]]))
        if (length(combined) == 0) combined <- NULL
        if (length(warnsAndError) == 0) warnsAndError <- NULL
        c(combined, warnsAndError)
       }, mergeList(topLevelElements)$compare,
          which(names(topLevelElements) == "compare"), SIMPLIFY = FALSE)

    # Get unpaired results
    testUnpairedTypes <- unlist(xmlApply(comparisonTree[[length(
                                         topLevelElements)]][["test"]],
                                         xmlName), use.names = FALSE)
    testUnpaired <- lapply(testUnpairedTypes, function(type)
        mergeList(xmlApply(comparisonTree[[length(topLevelElements)]][[
                                           "test"]][[type]], xmlValue)))
    names(testUnpaired) <- testUnpairedTypes

    controlUnpairedTypes <- unlist(xmlApply(comparisonTree[[length(
                                            topLevelElements)]][["control"]],
                                            xmlName), use.names = FALSE)
    controlUnpaired <- lapply(controlUnpairedTypes, function(type)
        mergeList(xmlApply(comparisonTree[[length(topLevelElements)]][[
                                           "control"]][[type]], xmlValue)))
    names(controlUnpaired) <- controlUnpairedTypes

    # If the unpaireds are just list() or blank, change them to NULL
    if (length(testUnpaired) == 0) testUnpaired <- NULL
    if (length(controlUnpaired) == 0) controlUnpaired <- NULL

    # Combine all the results
    logQCResult <- list(info = info, testInfo = testInfo,
                          controlInfo = controlInfo, results =
                          c(filetypeResults, list(unpaired =
                          c(list(test = testUnpaired,
                            control = controlUnpaired)))))
    class(logQCResult) <- "qcCompareExprResult"
    return(logQCResult)
}

# --------------------------------------------------------------------
#
# readCompareFunLog()
#
# --------------------------------------------------------------------
"readCompareFunLog" <- function(logFile, logClass) {
    exprResultsRoot <- xmlRoot(xmlTreeParse(logFile))
    path <- as.character(xmlAttrs(exprResultsRoot))
    exprResults <- unlist(lapply(xmlChildren(exprResultsRoot),
                                 xmlValue))
    names(exprResults) <- NULL
    funResults <- lapply(exprResults, readCompareExprLog)
    attr(funResults, "path") <- path
    class(funResults) <- logClass
    funResults
}

