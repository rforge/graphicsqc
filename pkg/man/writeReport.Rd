\name{writeReport}
\alias{writeReport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ writeReport }
\description{
  Will produce a HTML report of any differences found from the compare
  function.
}
\usage{
  writeReport(qcResult, xslStyleSheets = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qcResult}{either a character vector specifying the path to the log
                  file to report on, or the folder where it will report on
                  all the log files or an R object of class qcPlotExpr,
                  qcCompareExpr, etc. to create the report.}
  \item{xslStyleSheets}{a named list specifying which XSL style sheets
                        to override. Can override any of: "plotExprStyleSheet",
                        "plotFunAndFileStyleSheet", "compareExprStyleSheet",
                        and "compareFunAndFileStyleSheet". If none are
                        specified, the default ones are used.}
}
\details{
  Reports are made for all log files that the given log file might refer to.
  A character vector is returned containing paths to the log files (with only
  the highest classed ones returned when a folder is given).  
}
\value{
  A character vector giving the location to the highest classed object(s)
  which were reported on (See the note).
}
\note{
  Currently, if the R object is given, the log file and sub-class
  log files must exist (xsltApplyStyleSheet requires a filename or a string
  containing the doc)
}
\seealso{\code{\link{plotExpr}}, \code{\link{plotFun}}, \code{\link{plotFile}}
, \code{\link{compare}},
}
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
