# --------------------------------------------------------------------
# report.R
# --------------------------------------------------------------------

# --------------------------------------------------------------------
#
# writeReport()
#
# writeReport will produce a HTML output of any differences found
# from the compare function.
#
# 'comparison' is either a character vector specifying the path to the log
# file to report on, or the folder where it will report on all the log files,
# or an R object to create the report.
# Reports are made for all log files that the given log file might refer to.
# A character vector is returned containing paths to the log files (with only
# the highest classed ones returned when a folder is given).
#
# Note: Currently, if the R object is given, the log file and sub-class
# log files must exist (xsltApplyStyleSheet requires a filename or a string
# containing the doc)
# --------------------------------------------------------------------
"writeReport" <- function(qcResult, xslStyleSheets = NULL) {
    library(Sxslt)
    #SxsltInitializationFunction()
    ## Note: There is a call to browser() in addXSLTFunctions...
    # -- can't just give logToHTML even if it is defined outside - have
    # to give the function definition here?
    addXSLTFunctions(logToHTML = function(...) logNameToHTML(file.path(...)),
                     getCompareExprName =
                       function(logWithPath) {
                           strsplit(basename(logWithPath),
                                    "-compareExprLog.xml")[[1]]
                       }
                    )
    xslStyles <- list("plotExprStyleSheet" =
                      system.file("xsl", "plotExpr.xsl",
                                  package = "graphicsqc"),
                      "plotFunAndFileStyleSheet" =
                      system.file("xsl", "plotFunAndFile.xsl",
                                  package = "graphicsqc"),
                      "compareExprStyleSheet" =
                      system.file("xsl", "compareExpr.xsl",
                                  package = "graphicsqc"),
                      "compareFunAndFileStyleSheet" =
                      system.file("xsl", "compareFunAndFile.xsl",
                                  package = "graphicsqc"))
    xslStyles[names(xslStyleSheets)] <- xslStyleSheets

    if (is.character(qcResult)) {
        # Either a folder or a file.
        fileInfo <- file.info(qcResult)
        if (is.na(fileInfo$isdir)) {
            stop("file ", dQuote(qcResult), " not found", call. = FALSE)
        } else if (!fileInfo$isdir) {
            # It's a file
            qcResult <- readLog(qcResult)
        } else {
            # It's a PATH (not a file) !
            # so autodetect log files
            ## .
        }
    }

    # Now we have the highest level R object we want to report on.

    if (inherits(qcResult, "qcPlotExprResult")) {
        plotExprPath <- file.path(qcResult[["info"]][["directory"]],
                                  qcResult[["info"]][["logFilename"]])
        plotExpr <- xsltApplyStyleSheet(plotExprPath,
                                        xslStyles[["plotExprStyleSheet"]])
        logName <- logNameToHTML(plotExprPath)
        saveXML(plotExpr$doc, file = logName)
        return(logName)
        
    } else if (inherits(qcResult, "qcPlotFunResult") ||
               inherits(qcResult, "qcPlotFileResult")) {
        plotFPath <- file.path(qcResult[["info"]][["directory"]],
                                 qcResult[["info"]][["logFilename"]])
        plotF <- xsltApplyStyleSheet(plotFPath,
                                     xslStyles[["plotFunAndFileStyleSheet"]])
        logName <- logNameToHTML(plotFPath)
        saveXML(plotF$doc, file = logName)
        return(logName)

    } else if (inherits(qcResult, "qcCompareExprResult")) {
        compareExprPath <- file.path(qcResult[["info"]][["path"]],
                                     qcResult[["info"]][["logFilename"]])
        testPath <- file.path(qcResult[["testInfo"]][["directory"]],
                              qcResult[["testInfo"]][["logFilename"]])
        controlPath <- file.path(qcResult[["controlInfo"]][["directory"]],
                                 qcResult[["controlInfo"]][["logFilename"]])

        compareExpr <- xsltApplyStyleSheet(compareExprPath,
                                          xslStyles[["compareExprStyleSheet"]])
        testExpr <- xsltApplyStyleSheet(testPath,
                                        xslStyles[["plotExprStyleSheet"]])
        controlExpr <- xsltApplyStyleSheet(controlPath,
                                           xslStyles[["plotExprStyleSheet"]])
        logName <- logNameToHTML(compareExprPath)
        saveXML(compareExpr$doc, file = logName)
        saveXML(testExpr$doc, file = logNameToHTML(testPath))
        saveXML(controlExpr$doc, file = logNameToHTML(controlPath))
        return(logName)
        
    } else if (inherits(qcResult, "qcCompareFunResult") ||
               inherits(qcResult, "qcCompareFileResult")) {
        lapply(qcResult[["results"]], writeReport, xslStyles)
        writeReport(qcResult[["info"]][["testLog"]], xslStyles)
        writeReport(qcResult[["info"]][["controlLog"]], xslStyles)
        compareFPath <- paste(qcResult[["info"]][["path"]],
                                qcResult[["info"]][["logFilename"]],
                                sep = .Platform$file.sep)
        compareF <- xsltApplyStyleSheet(compareFPath,
                                    xslStyles[["compareFunAndFileStyleSheet"]])
        logName <- logNameToHTML(compareFPath)
        saveXML(compareF$doc, file = logName)
        return(logName)
        
    } else {
        stop("either ", sQuote("qcResult"), " is not a valid qcResult, ",
             "or that type is not supported yet", call. = FALSE)
    }
}

"logNameToHTML" <- function(logName) gsub("[.]xml$", ".html", logName)
"logToHTML" <- function(...) logNameToHTML(file.path(...))
"getCompareExprName" <- function(logWithPath) {
    strsplit(basename(logWithPath), "-compareExprLog.xml")[[1]]
}

