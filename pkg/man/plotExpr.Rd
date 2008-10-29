\name{plotExpr}
\alias{plotExpr}
\alias{plotFile}
\alias{plotFunction}
\alias{plotPackage}
\title{ Plot arbitrary code }
\description{
  Produce plots from R expression(s), function(s), or file(s) in
  specified file formats. An XML file is also created which contains
  information about the plots produced.
}
\usage{
  plotExpr(expr, filetype = NULL, path = NULL, prefix = NULL,
           clear = FALSE)

  plotFile(filename, filetype = NULL, path = NULL, prefix = basename(filename),
           clear = FALSE)

  plotFunction(fun, filetype = NULL, path = NULL, prefix = fun,
               clear = FALSE)
}
\arguments{
  \item{expr}{character vector of R expressions which may or may not
    produce graphical output.}
  \item{filename}{the name of the file which the expressions are to be
    read from. The path is assumed to be relative to the
    current working directory unless an absolute path is given.}
  \item{fun}{character vector naming the function(s) to plot or just the
    (named) function.}
  \item{filetype}{character vector specifying file formats to produce the
    plots in (see details for currently supported formats).}
  \item{path}{character vector; path to produce output in. If not given, the
    current working directory will be used.}
  \item{prefix}{character vector; prefix for files produced. If multiple
    functions or files are given, the resulting plotFile or plotFunction
    XML file will use the first prefix.}
  \item{clear}{logical (not NA); remove files with names we might use
    first. If \code{clear} is \code{FALSE} and files exist
    with names we might use, an error is given.}
}
\details{
  All functions evaluate the code they are given, capturing and recording any
  warnings and errors. The code run for plotFunction is extracted from
  any example code (see \code{\link{example}}).

  If an error is encountered when running a block of code, that
  particular block (say, a file or function) will stop being executed
  for that filetype but the error will be recorded. If a warning is
  encountered, the code will continue being evaluated.

  The name for the log file is based on the first prefix and the type of
  function producing the plots. For \code{plotExpr} XML logs, 1 log file
  will be produced with a name like \sQuote{prefix-log.xml}.
  \code{plotFile} and \code{plotFunction} work by making multiple calls
  to \code{plotExpr}, so will produce \code{plotExpr} logs (one for each
  file or function respectively), as well as their own log, which will
  be named (using the first prefix) like \sQuote{prefix-fileLog.xml} and
  \sQuote{prefix-funLog.xml} respectively.

  Currently supported file formats are \sQuote{pdf}, \sQuote{png}, and
  \sQuote{ps}.

}
\value{
  A list of class \code{qcPlotExprResult}, \code{qcPlotFileResult}, or
  \code{qcPlotFunctionResult} respectively. The list contains
  information about the environment creating the plots (Operating
  System, R version, date, call), the names of the plots produced and
  any warnings/errors produced.
}
\section{Warning}{
  Do not give any code that will open a graphics device (especially if
  that device is not closed).
}
\seealso{
  \code{\link{compare}}, \code{\link{writeReport}}.
}
\examples{
\dontrun{
  # plotExpr example:
  example1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                       c("pdf", "ps"), "example1", "myPrefix")
  # There should now be a folder "example1" in the current
  # working directory containing pdf and ps files and myPrefix-log.xml, ie
  list.files("example1")

  # plotFunction example:
  example2 <- plotFunction(c("plot", "barplot", "hist"), c("pdf", "ps"),
                           path = "example2")
  list.files("example2")

  # A bigger example:
  # require(grid)
  # gridExample <- plotFunction(ls("package:grid"), c("pdf", "png", "ps"),
  #                             path = "gridExample")
}
}
\keyword{ utilities }
