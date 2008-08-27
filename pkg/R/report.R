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
# --------------------------------------------------------------------
"writeReport" <- function(){}


# --------------------------------------------------------------------
#
# readCompareExprLog()
#
# --------------------------------------------------------------------
"readCompareExprLog" <- function(filename) {
    library(XML)
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
        resultDiffPlot <- xmlApply(comparisonTree[[i]], function(tree) {
                                   xmlApply(tree, xmlValue) })
        combined <- lapply(seq_len(length(comparisonTree[[i]])), function(j)
                           c(controlAndTest[[j]], resultDiffPlot[[j]]))
        if (length(combined) == 0) combined <- NULL
        combined
       }, mergeList(topLevelElements)$compare,
          which(names(topLevelElements) == "compare"), SIMPLIFY = FALSE)
    
    # Get unpaired results
    testUnpairedTypes <- unlist(xmlApply(comparisonTree[[length(topLevelElements)]][["test"]], xmlName), use.names = FALSE)
    testUnpaired <- lapply(testUnpairedTypes, function(type)
        mergeList(xmlApply(comparisonTree[[length(topLevelElements)]][["test"]][[type]], xmlValue)))
    names(testUnpaired) <- testUnpairedTypes
    
    controlUnpairedTypes <- unlist(xmlApply(comparisonTree[[length(topLevelElements)]][["control"]], xmlName), use.names = FALSE)
    controlUnpaired <- lapply(controlUnpairedTypes, function(type)
        mergeList(xmlApply(comparisonTree[[length(topLevelElements)]][["control"]][[type]], xmlValue)))
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
    exprResults <- unlist(lapply(xmlChildren(xmlRoot(xmlTreeParse(logFile))),
                                 xmlValue))
    names(exprResults) <- NULL
    funResults <- lapply(exprResults, readCompareExprLog)
    class(funResults) <- logClass
    funResults
}

