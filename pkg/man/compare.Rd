\name{compare}
\alias{compare}
\alias{compareExpr}
\alias{compareFun}
\alias{compareFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ compare }
\description{
  Takes the results from plotExpr or plotFun etc. and compares the files for
  differences. If ImageMagick is installed it will also produce plots of the
  differences.
}
\usage{
compare(test, control, logDiffDirectory = test$info$directory, 
        erase = c("none", "identical", "files", "all"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test, control}{An R object of class qcPlotExpr, qcPlotFun, etc. or the
            path to the XML file produced by any of the associated functions.}
  \item{logDiffDirectory}{character vector; specifies where all the diff
        output (.diff files, plots, and comparison logs) should be placed.}
  \item{erase}{character vector; one of '"none"', '"identical"', '"files"',
               '"all"'}
}
\details{
  Compares the files using GNU diff and ImageMagick if it is installed. ..
  'erase' of "none" means don't erase any files; "identical" means only erase
  files which were found to be identical; "files" means all the associated
  plots (in the test directory) but not the log files; "all" means erase
  everything (plots and any diff files or diff plots produced).
  
  When comparing qcPlotFun or qcPlotFile results, the name of the log file will
  be the prefix of the first test log file with "-compareFunLog.xml" appended
  on the end. It will be placed in the logDiffDirectory.
}
\value{
  A list of class qcCompareExpr or qcCompareFun containing the results of the
  comparisons.
}
\note{
  GNU diff must be installed on the system. ImageMagick will be used if it is
  installed.
  'compareFun' and 'compareFile' use the first test file prefix for the prefix
  of the resulting comparison.
}
\seealso{\code{\link{plotExpr}}, \code{\link{plotFun}}, \code{\link{plotFile}}
  , \code{\link{writeReport}}}
\examples{
#test1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
#                  c("pdf", "ps"), "test", "testdir1", FALSE)
# compare..
# There should now be a folder "testdir1" in getwd() containing
# pdf and ps files and test-log.xml, ie
#list.files("testdir1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
