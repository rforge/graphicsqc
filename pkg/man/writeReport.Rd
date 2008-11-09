\name{writeReport}
\alias{writeReport}
\title{ Generate a HTML report based on plots or comparisons }
\description{
  Will produce a HTML report of the results from any of the
  \code{qcPlot*}, or \code{qcCompare*} results.
}
\usage{
  writeReport(qcResult, xslStyleSheets = NULL)
}
\arguments{
  \item{qcResult}{one of:
    \itemize{
      \item an R object of class \code{qcPlot*}, or \code{qcCompare*}.
      \item the path to the log file to report on
      \item a path to the directory, where the highest classed log file
        will be auto-detected and then reported on (note that first
        comparison logs are searched for, then plot logs).
    }
  }
  \item{xslStyleSheets}{a named list specifying which XSL style sheets
    to override by giving the name of the style sheet to override, and
    the location of the xsl file. Can override any of:
    \dQuote{plotExprStyleSheet}, \dQuote{plotFunAndFileStyleSheet},
    \dQuote{compareExprStyleSheet}, and \dQuote{compareFunAndFileStyleSheet}.
    If none are specified, the default (system) ones are used.
  }
}
\details{
  When reporting on an object, all further qcPlot* or qcCompare* files
  which the current object refers to are also reported on. This is so
  that full information reports can be given, along with individual
  breakdowns. In order for this to happen, all log files that
  the object currently being reported on refers to must exist, as well
  as any subsequent log files that they refer to.

  All reports are placed in the same directory as the XML file they
  refer to, with the same name, except with the extension changed from
  \sQuote{.xml} to \sQuote{.html}.
}
\value{
  A character vector giving the (absolute) path of the highest classed
  object reported on. Comparison logs are considered higher classed than
  plot logs.
}
\seealso{
  \code{\link{plotExpr}}, \code{\link{plotFile}},
  \code{\link{plotFunction}}, \code{\link{compare}}.
}
\examples{
\dontrun{
  # After running the `?compare' example
  writeReport(compExpr)

  # Showing how to overwrite stylesheets
  # writeReport(compExpr, list(compareExprStyleSheet="~/myCompareExpr.xsl"))
}
}
\keyword{ utilities }
