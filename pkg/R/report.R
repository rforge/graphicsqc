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
"writeReport" <- function(qcResult) {
    #SxsltInitializationFunction()
    ## Note: There is a call to browser() in addXSLTFunctions...
    addXSLTFunctions(getLogName = function(...) logNameToHTML(file.path(...)))
    
    plotExprStyleSheet <- system.file("xsl", "plotExpr.xsl",
                                      package = "graphicsqc")
    compareExprStyleSheet <- system.file("xsl", "compareExpr.xsl",
                                         package = "graphicsqc")
    
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

    if (inherits(qcResult, "qcPlotExprResult")) {
        plotExprPath <- file.path(qcResult[["info"]][["directory"]],
                                  qcResult[["info"]][["logFilename"]])
        plotExpr <- xsltApplyStyleSheet(plotExprPath, plotExprStyleSheet)
        logName <- logNameToHTML(plotExprPath)
        saveXML(plotExpr$doc, file = logName)
        return(logName)
        
    } else if (inherits(qcResult, "qcCompareExprResult")) {
        compareExprPath <- file.path(qcResult[["info"]][["logDiffDirectory"]],
                                     qcResult[["info"]][["logFilename"]])
        testPath <- file.path(qcResult[["testInfo"]][["directory"]],
                              qcResult[["testInfo"]][["logFilename"]])
        controlPath <- file.path(qcResult[["controlInfo"]][["directory"]],
                                 qcResult[["controlInfo"]][["logFilename"]])

        compareExpr <- xsltApplyStyleSheet(compareExprPath,
                                           compareExprStyleSheet)
        testExpr <- xsltApplyStyleSheet(testPath, plotExprStyleSheet)
        controlExpr <- xsltApplyStyleSheet(controlPath, plotExprStyleSheet)
        logName <- logNameToHTML(compareExprPath)
        saveXML(compareExpr$doc, file = logName)
        saveXML(testExpr$doc, file = logNameToHTML(testPath))
        saveXML(controlExpr$doc, file = logNameToHTML(controlPath))
        return(logName)
        
    } else {
        stop("either ", sQuote("qcResult"), " is not a valid qcResult, ",
             "or that type is not supported yet", call. = FALSE)
    }
}

"logNameToHTML" <- function(logName) gsub("[.]xml$", ".html", logName)
