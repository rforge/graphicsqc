\name{graphicsQC-package}
\alias{graphicsQC-package}
\alias{graphicsQC}
\alias{graphicsqc}
\docType{package}
\title{
  Quality Control for Graphics
}
\description{
  Generates graphics files, compares them with "model" files,
  and reports the results.
}
\details{
  \tabular{ll}{
    Package: \tab graphicsQC\cr
    Type: \tab Package\cr
    Version: \tab 0.9\cr
    Date: \tab 2008-10-30\cr
    License: \tab GPL-2\cr
  }
  To generate files, use \code{\link{plotExpr}},
  \code{\link{plotFile}}, or \code{\link{plotFunction}}.
  To compare sets of these, use \code{\link{compare}}.
  To generate a report based on the comparison, use
  \code{\link{writeReport}}.

  One possible way of using these functions is to create a set of plots in
  a directory in an old version of R (say, the control group) using one
  of the plotting functions. Then to load a new version of R and create
  the same plots in a different directory (say, the test group). A
  comparison can then be done by specifying the control and test
  directories. Then a report can be made on the comparison object.

  It is highly recommended to use separate directories for the test and
  control. If the same directory is used for both, all the prefixes in
  the test and all the prefixes in the control must be unique, and
  auto-detect will not work if the same directory is given twice.  
}
\author{
  Stephen Gardiner
}
\references{
  Free Software Foundation, Inc. 2008 \emph{Diffutils}.
  \url{http://www.gnu.org/software/diffutils/diffutils.html}

  ImageMagick Studio LLC. 2008 \emph{ImageMagick}.
  \url{http://www.imagemagick.org/}

  Murrell, P. & Hornik, K. 2003 \emph{Quality Assurance for Graphics in \R}
  \url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003/Proceedings/MurrellHornik.pdf}.
}
\seealso{
  \code{\link{plotExpr}}, \code{\link{plotFile}},
  \code{\link{plotFunction}}, \code{\link{compare}},
  \code{\link{writeReport}}
}
\examples{
\dontrun{
  # Create some plots to compare (1st and 3rd plots have differences)
  comp1 <- plotExpr(c("plot(1:10)", "plot(4:40)", "x<-3", "plot(2:23)"),
                    c("pdf", "ps"), "myPrefix", "comp1")
  comp2 <- plotExpr(c("plot(1:11)", "plot(4:40)", "x<-3", "plot(5:15)"),
                    c("pdf", "ps"), "myPrefix", "comp2")

  # Compare them
  compExpr <- compare(comp1, comp2)

  # Write a HTML report
  writeReport(compExpr)
}
}
\keyword{utilities}
