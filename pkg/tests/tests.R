# ------------------- Testing makeplots.R -------------------------
# General Tests:
# plotExpr takes arguments; expr, filetype, prefix, path, clear
test1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                  c("pdf", "ps"), "test", "testdir1", FALSE)
# There should now be a folder "testdir1" in getwd() containing
# pdf and ps files and test-log.xml, ie
list.files("testdir1")

# Repeating the same again should give an error as clear is False
plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                c("pdf","ps"), "test", "testdir1", FALSE)
# Error: files of intended filename already exist in ‘path’

# plotFunction takes same arguments but prefix defaults to fun
test2 <- plotFunction(c("plot", "lm"), c("pdf", "ps"), path="testdir2",
                      clear=FALSE)
# Folder with output is produced in testdir2, with plot-log.xml,
# lm-log.xml, and plot-lm-funLog.xml
list.files("testdir2")

## --Insert tests for plotPackage when done

## --Insert tests for plotFile when done

# Testing evalPlotCode with badExpression
badExpression <- c("x<-3", "plot(x:8)", "warning(\"firstWarning\")",
          "warning(\"secondWarning\")", "stop(\"end error\")",
          "warning(\"NOTCALLED\")")
lapply(c("pdf", "ps"), evalPlotCode, badExpression, "prefix-%d")

# ------------------- Testing compare.R -------------------------
# --- Testing reading of XML log files ---
test1Check <- readPlotExprLog("testdir1/test-log.xml")
identical(test1, test1Check) # Should be True

test2Check <- readPlotFunLog("testdir2/plot-lm-funLog.xml")
identical(test2, test2Check) # Should be True

## --Insert test for readPlotPackageLog when done

## -- Insert test for readPlotFileLog when done

# --- Testing compare for plotExpr ---
# Same as test1 but missing the last plot
test1B <- plotExpr(c("plot(1:10)", "plot(4:40)"), c("pdf", "ps"), 
                         "test2", "testdir1B", FALSE)
compare(test1, test1B, "none") # No XML output yet. Should all be identical
                               # but with unpaired files

# Second plot is different, third plot is still unpaired, test1B will be wiped
# with clear being TRUE
test1C <- plotExpr(c("plot(1:10)", "plot(4:41)"), c("pdf", "ps"), 
                         "test2", "testdir1B", TRUE)
compare(test1, test1C, "none")

# --- Testing compare for plotFun ---
# All plots from plot are different to barplot
testFun <- plotFunction(c("plot"), c("pdf", "ps"), path="testFun", clear=FALSE)
testFun2 <- plotFunction(c("barplot"), c("pdf", "ps"), path="testFun2",
                                                                   clear=FALSE)
funComparison <- compare(testFun, testFun2, "none")
funComparison[,1]
# Note: Diff plots are being stored in the test directory, so
list.files("testFun")

# Now delete testdir* ? testFun*   And also prefix-1.pdf and prefix-1.ps




