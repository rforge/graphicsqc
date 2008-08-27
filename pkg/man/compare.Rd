\name{compare}
\alias{compare}
\alias{compareExpr}
\alias{compareFun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ compare }
\description{
Takes the filename/path of a file containing R code and produces plots in
specific file formats of the intended plot(s).
}
\usage{
compare(test, control, erase=c("none", "identical", "files", "all"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test, control}{An R object of class qcPlotExpr, qcPlotFun, etc. or the
            path to the XML file produced by any of the associated functions.}
  \item{erase}{character vector; one of '"none"', '"identical"', '"files"',
               '"all"'}
}
\details{
  Compares the files using GNU diff and ImageMagick if it is installed. ..
  'erase' of "none" means don't erase any files; "identical" means only erase
  files which were found to be identical; "files" means all the associated
  plots but not the log files; "all" means erase everything (plots and any
  diff files or diff plots produced).
  
  When comparing qcPlotFun or qcPlotFile results, the name of the log file will
  be the prefix of the first test log file with "-compareFunLog.xml" appended
  on the end. It will be placed in the directory of the first test.
}
\value{
  A list of class qcCompareExpr or Fun containing the results of the
  comparisons.
}
\note{
  GNU diff must be installed on the system. ImageMagick will be used if it is
  installed.
}
\seealso{\code{\link{plotExpr}}, \code{\link{plotFun}}, \code{\link{plotFile}}}
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
