\name{plotFile}
\alias{plotFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plotFile }
\description{
Takes the filename/path of a file containing R code and produces plots in
specific file formats of the intended plot(s).
}
\usage{
plotFile(filename, filetype = NULL, prefix = basename(filename), path = NULL,
 clear = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{character vector of filename or path of R code to plot}
  \item{filetype}{character vector specifying file formats to produce the
                  plots in.}
  \item{prefix}{character vector; prefix for files produced.}
  \item{path}{character; path to use.}
  \item{clear}{logical (not NA); remove files with names we might use first.}
}
\details{
  Will produce plots... If path is not given, will default to the current
  working directory.
}
\value{
  A list of class qcPlotFileResult containing the resulting plots.
}
\warning{
  Don't give any code that will open a graphics device.
}
\note{
  If multiple functions
  or multiple prefixes are given, the resulting plotFile xml file's prefix will
  only be the first, and the resulting file will also be placed in the first
  file's directory.
}
\seealso{\code{\link{plotFun}}, \code{\link{plotExpr}}}
\examples{
#test1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
#                  c("pdf", "ps"), "test", "testdir1", FALSE)
# There should now be a folder "testdir1" in getwd() containing
# pdf and ps files and test-log.xml, ie
#list.files("testdir1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
