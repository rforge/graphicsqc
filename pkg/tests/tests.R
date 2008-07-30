# ------------------- Testing makeplots.R -------------------------
library(graphicsqc)
# General Tests:
# plotExpr takes arguments; expr, filetype, prefix, path, clear
test1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                  c("pdf", "ps"), "test", "testdir1", FALSE)
# There should now be a folder "testdir1" in getwd() containing
# pdf and ps files and test-log.xml, ie
list.files("testdir1")

# plotExpr() allows expressions as well as text
  # ^^ i.e. like this ?
expressions <- expression({ plot(1:10); plot(4:40); x<-3; plot(2:23) })
test1b <- plotExpr(expressions, "png", "exprTest", "exprDir", FALSE)

# Repeating the same again should give an error as clear is False
try(plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                c("pdf","ps"), "test", "testdir1", FALSE))
# Error: files of intended filename already exist in ‘path’

# plotFunction takes same arguments but prefix defaults to fun
test2 <- plotFunction(c("plot", "lm"), c("pdf", "ps"), path="testdir2",
                      clear=FALSE)
# Folder with output is produced in testdir2, with plot-log.xml,
# lm-log.xml, and plot-lm-funLog.xml
list.files("testdir2")

## --Insert tests for plotFile when done
plotFiletest <- plotFile(c("Rfile.txt", "Rfile2.txt"), "png", path =
                         c("filedir1", "filedir2"))
     # Note: If only 1 path is given, it will re-use it. Is this desired?  YES!
     # Also note the list of length 2 as the result
list.files("filedir1")
list.files("filedir2")

## --Insert tests for plotPackage when done

# Testing evalPlotCode with badExpression
badExpression <- c("x<-3", "plot(x:8)", "warning(\"firstWarning\")",
          "warning(\"secondWarning\")", "stop(\"end error\")",
          "warning(\"NOTCALLED\")")
lapply(c("pdf", "ps"), evalPlotCode, badExpression, "prefix-%d")

# ------------------- Testing compare.R -------------------------
# --- Testing reading of XML log files ---
test1Check <- readPlotExprLog(file.path("testdir1", "test-log.xml"))
identical(test1, test1Check) # Should be True

test2Check <- readPlotFunLog(file.path("testdir2", "plot-lm-funLog.xml"))
identical(test2, test2Check) # Should be True

test2AutoCheck <- getQCResult("testdir2") # Testing auto-detect
identical(test2, test2AutoCheck)

## --Insert test for readPlotPackageLog when done

## -- Insert test for readPlotFileLog when done
   #x readPlotFileLog just produces a qcPlotExpr result

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
funComparison[[1]]
# Note: Diff plots are being stored in the test directory, so
list.files("testFun")

# Now delete testdir* ? testFun*   And also prefix-1.pdf and prefix-1.ps

## Something to note: Doing something like
# y <- 1:10
# x <- rnorm(10)
# myLm <- lm(y~x)
# plotExpr("plot(myLm)", "png", "testlm", "testlm", FALSE)
# Will require the user to keep doing "Hit <Return> to see next plot:"
# if not in batch mode
# FULL fix requires change to source cope of plot.lm
# WORKAROUND is just use BATCH mode !



