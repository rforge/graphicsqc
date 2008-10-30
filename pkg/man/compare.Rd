\name{compare}
\alias{compare}
\alias{compareExpr}
\alias{compareFun}
\alias{compareFile}
\title{ Compare graphics output }
\description{
  Compares plots/warnings/errors from \code{\link{plotExpr}},
  \code{\link{plotFile}}, or \code{\link{plotFunction}}.
  For the text-based formats (i.e. pdf or ps), a .diff file is created.
  If ImageMagick is installed, plots of the differences will also be
  produced.
}
\usage{
compare(test, control, path = test$info$directory, 
        erase = c("none", "identical", "files", "all"))
}
\arguments{
  \item{test, control}{either:
    \itemize{
      \item R objects of class \code{qcPlotExpression},
        \code{qcPlotFile}, or \code{qcPlotFunction}.
      \item Character vectors of the paths to the respective files,
        where relative paths are assumed unless an absolute path is given.
      \item Character vectors of the directories which contain the log
        files to compare. The highest classed object in the folder will
	be chosen for comparison (i.e. if a plotFunction log is in the
	directory and many plotExpr logs, all of the plotExpr logs will
	be assumed to belong to the plotFunction log).
    }
    The specification for \code{test} and \code{control} can be mixed and
    matched, as long as the resulting objects are of the same class.      
  }
  \item{path}{character vector; specifies where all the diff
    output (.diff files, plots of differences, and comparison logs)
    should be placed.
  }
  \item{erase}{character vector; one of \code{"none"},
    \code{"identical"}, \code{"files"}, or \code{"all"}.
    \describe{
      \item{\code{"none"}}{do not delete anything.}
      \item{\code{"identical"}}{delete plots in the \code{test}
	directory which were identical.}
      \item{\code{"files"}}{delete all plots (and .diff files) in the
	\code{test} directory (leaving only the log files).}
      \item{\code{"all"}}{delete all files created in the
	\code{test} directory and then the directory if it is empty}
    }
    Currently only \code{"none"} is fully supported.
  }
}
\details{
  Plots are compared using \acronym{GNU} diff. If a difference is
  detected and the current filetype being compared is a text-based
  format, a .diff file will be produced. If ImageMagick is installed,
  plots of differences will also be created.

  It is possible for some plots to appear say, in the test group, but
  not in the control group (i.e. the function \code{plot} has an extra
  example plot in a new version of R). These such plots are classified
  as \sQuote{unpaired}. Unpaired files do not have a corresponding plot
  to compare with so are separated into an unpaired section. It is also
  possible for entire filetypes to be unpaired. Currently if there is a
  completely unpaired function or file when trying to compare, recycling
  will be used. This is intended to change in the future.

  In many instances, it is also useful to know whether there is any
  change in warnings or errors. If any difference is detected in the
  warnings/errors for a filetype, all of the warnings or errors
  (whichever had the difference detected) for that filetype are
  given. It is then up to the user to decide what the difference is
  (i.e. whether the ordering has changed or if one group has an extra
  warning etc.).

  For each set of plot-logs being compared, a comparison log will be
  produced. So for each pair of qcPlotExprResult logs being compared, a
  comparison log will be produced with a name like
  \sQuote{testPrefix+controlPrefix-compareExprLog.xml}. When comparing
  qcPlotFileResults or qcPlotFunctionResults there will also be a
  compareFileLog or compareFunLog produced which will take a name like
  \sQuote{testPrefix+controlPrefix-compareFunLog.xml}, where the
  testPrefix and controlPrefix are chosen from the first prefixes in the
  set of compareExprLogs being compared (which in turn come from the first
  plotExpr logs). These logs are placed in \code{path}.
  
}
\value{
  A list of class \code{qcCompareExprResult}, \code{qcCompareFileResult} or
  \code{qcCompareFunResult} containing the results of the comparisons.

  \code{qcCompareExprResult} files contain a list of info about the Operating
  System, R version, date, call, the info from the test, info from the
  control, and then information about the results of the comparisons
  (results by filetype giving the result, names of diff files and plots of
  differences if produced), including any unpaired plots or filetypes
  (with corresponding warnings/errors).

  For \code{qcCompareFile} or \code{qcCompareFun} an initial info
  section is included, followed by a list containing each individual
  \code{qcCompareExprResult}.
}
\note{
  \acronym{GNU} diff must be installed on the system. ImageMagick is not
  necessary, but greatly extends functionality.
}
\seealso{
  \code{\link{plotExpr}}, \code{\link{plotFile}},
  \code{\link{plotFunction}}, \code{\link{writeReport}}}
\examples{
\dontrun{
  # Create sets to compare (1st and 3rd are different)
  comp1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                    c("pdf", "ps"), "myPrefix", "comp1")
  comp2 <- plotExpr(c("plot(1:11)", "plot(4:40)", "x<-3", "plot(5:15)"),
                    c("pdf", "ps"), "myPrefix", "comp2")
  compExpr <- compare(comp1, comp2)
  # All the diff output has been placed in "comp1" (the test directory)
  compExpr
  # For a better way of viewing this, see ?writeReport
}
}
\keyword{ utilities }
