<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:r="http://www.r-project.org"
                exclude-result-prefixes="r"
                extension-element-prefixes="r">
<xsl:output method="html" indent="yes"/>

<!-- Template for qcCompareFunResult -->
<xsl:template match="/qcCompareFunResult">
  <xsl:variable name="docs" select="qcCompareExprResult"/>
  <xsl:variable name="numDiffFuns"
                select="count(document($docs)/qcCompareExprResult[compare/
                        comparison[result='different']])"/>
  <xsl:variable name="numDiffPlots"
                select="count(document($docs)/qcCompareExprResult/compare/
                        comparison[result='different'])"/>
  <xsl:variable name="numIdenticalFuns"
                select="count(document($docs)/qcCompareExprResult[compare/
                        comparison[result='identical']])"/>
  <xsl:variable name="numIdenticalPlots"
                select="count(document($docs)/qcCompareExprResult/compare/
                        comparison[result='identical'])"/>
  <xsl:variable name="numFunctionsNoPlots"
                select="count(document($docs)[not(qcCompareExprResult/
                        compare/node())])"/>
  <html>
  <head>
    <title>Compare Function Result</title>
    <style type="text/css">
      table {border: 1px solid #585858;
             background-color: #D8D8D8;}
      th {background-color: #80add6;
          font-weight: bold;
          text-align: left;
          padding-left: 5px;
          padding-right: 5px;
          padding-top: 3px;
          padding-bottom: 3px;}
      td {background-color: #fdfad9;
          padding-left: 5px;
          padding-right: 5px;
          padding-top: 3px;
          padding-bottom: 3px;}
      .topAlign {vertical-align: top}
    </style>  
  </head>
  <body>
    <h1>Compare Function Result</h1>
    <!-- Table of contents -->
    <ol>
      <li>
        <a href="#info">Info</a>
      </li>
      <li>
        <a href="#plotComp">
          Plot Comparisons
        </a>
        <ul>
          <li>
            <a href="#diffPlots">Different Plots</a>
            <xsl:choose>
              <xsl:when test="$numDiffPlots = 0">
                [none]
              </xsl:when>
              <xsl:otherwise>
                [<xsl:value-of select="$numDiffPlots"/> in
                 <xsl:value-of select="$numDiffFuns"/>
                 function<xsl:if test="$numDiffFuns > 1">s</xsl:if>]
              </xsl:otherwise>
            </xsl:choose>
          </li>
          <li>
            <a href="#identicalPlots">Identical Plots</a>
            <xsl:choose>
              <xsl:when test="$numIdenticalPlots = 0">
                [none]
              </xsl:when>
              <xsl:otherwise>
                [<xsl:value-of select="$numIdenticalPlots"/> in
                 <xsl:value-of select="$numIdenticalFuns"/>
                 function<xsl:if test="$numIdenticalFuns > 1">s</xsl:if>]
              </xsl:otherwise>
            </xsl:choose>
          </li>
          <li>
            <a href="#noPlots">No plots to compare</a>
            <xsl:choose>
              <xsl:when test="$numFunctionsNoPlots = 0">
                [none]
              </xsl:when>
              <xsl:otherwise>
                [<xsl:value-of select="$numFunctionsNoPlots"/>
                function<xsl:if test="$numFunctionsNoPlots > 1">s</xsl:if>]
              </xsl:otherwise>
            </xsl:choose>
          </li>
        </ul>
      </li>
      <li><a href="#warnsErrors">Warnings/Errors Comparisons</a></li>
      <li><a href="#unpaired">Unpaired</a></li>
    </ol>
    <!-- Info -->
    <h2><a name="info">Info</a></h2>
    <table>
      <tr>
        <th></th>
        <th style="text-align: center">Test</th>
        <th style="text-align: center">Control</th>
        <th style="text-align: center">Comparison</th>
      </tr>
      <tr>
        <th>Version</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFunResult/info/Rver"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFunResult/info/Rver"/>
        <xsl:apply-templates select="info/Rver"/>
      </tr>
      <tr>
        <th>OS</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFunResult/info/OS"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFunResult/info/OS"/>
        <xsl:apply-templates select="info/OS"/>
      </tr>
      <tr>
        <th>Date</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFunResult/info/date"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFunResult/info/date"/>
        <xsl:apply-templates select="info/date"/>
      </tr>
      <tr>
        <th>Call</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFunResult/info/call"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFunResult/info/call"/>
        <xsl:apply-templates select="info/call"/>
      </tr>
      <tr>
        <th>Directory</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFunResult/info/directory"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFunResult/info/directory"/>
        <xsl:apply-templates select="info/path"/>
      </tr>
      <tr>
        <th>Log Filename</th>
        <td>
          <a href="{r:logToHTML(string(info/testLog))}">
            <!-- Either basename of this testLog, or go through the log to
                 it's own info -->
            <xsl:value-of select="r:call('basename', string(info/testLog))"/>
          </a>
        </td>
        <td>
          <a href="{r:logToHTML(string(info/controlLog))}">
            <!-- Either basename of this testLog, or go through the log to
                 it's own info -->
            <xsl:value-of select="r:call('basename',
                                         string(info/controlLog))"/>
          </a>
        </td>
        <td>
          <xsl:value-of select="info/logFilename"/>
        </td>
      </tr>
    </table>
    <!-- Plots -->
    <h2><a name="plotComp">Plot Comparisons</a></h2>
    <!-- Different Plots -->
    <h3><a name="diffPlots">Different plots</a></h3>
    <xsl:choose>
      <xsl:when test="$numDiffFuns > 0">
        <table>
          <tr>
            <th>Functions</th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
            <th>Diff output</th>
            <th>Plot of differences</th>
          </tr>
          <xsl:for-each select="$docs">
            <xsl:variable name="doc" select="."/>
            <xsl:for-each select="document(.)/qcCompareExprResult/
                                  compare[comparison[result='different']]">
            <xsl:variable name="comparePosition" select="position()"/>
              <xsl:for-each select="comparison[result='different']">
                <tr>
                  <xsl:if test="$comparePosition=1 and position()=1">
                    <td rowspan="{count(../../compare/
                                 comparison[result='different'])}">
                      <a href="{r:logToHTML(string($doc))}">
                        <xsl:value-of select="r:getCompareExprName(
                                              string($doc))"/>
                      </a>
                    </td>
                  </xsl:if>
                  <xsl:choose>
                    <xsl:when test="position()=1">
                      <td rowspan="{last()}">
                        <xsl:value-of select="../@type"/>
                      </td>
                      <xsl:apply-templates select="."/>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffFile))}">
                          <xsl:value-of select="diffFile"/>
                        </a>
                      </td>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffPlot))}">
                          <xsl:value-of select="diffPlot"/>
                        </a>
                      </td>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:apply-templates select="."/>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffFile))}">
                          <xsl:value-of select="diffFile"/>
                        </a>
                      </td>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffPlot))}">
                          <xsl:value-of select="diffPlot"/>
                        </a>
                      </td>
                    </xsl:otherwise>
                  </xsl:choose>
                </tr>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No differences were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Identical plots -->
    <h3><a name="identicalPlots">Identical plots</a></h3>
    <xsl:choose>
      <xsl:when test="$numIdenticalFuns > 0">
        <table>
          <tr>
            <th>Functions</th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
          </tr>
          <xsl:for-each select="$docs">
            <xsl:variable name="doc" select="."/>
            <xsl:for-each select="document(.)/qcCompareExprResult/
                                  compare[comparison[result='identical']]">
            <xsl:variable name="comparePosition" select="position()"/>
              <xsl:for-each select="comparison[result='identical']">
                <tr>
                <xsl:if test="$comparePosition=1 and position()=1">
                  <td rowspan="{count(../../compare/
                               comparison[result='identical'])}">
                    <a href="{r:logToHTML(string($doc))}">
                      <xsl:value-of select="r:getCompareExprName(string(
                                            $doc))"/>
                    </a>
                  </td>
                </xsl:if>
                <xsl:choose>
                  <xsl:when test="position()=1">
                    <td rowspan="{last()}">
                      <xsl:value-of select="../@type"/>
                    </td>
                    <xsl:apply-templates select="."/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates select="."/>
                  </xsl:otherwise>
                </xsl:choose>
                </tr>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No identical plots were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- No plots to compare -->
    <h3><a name="noPlots">No plots to compare</a></h3>
    <xsl:choose>
      <xsl:when test="$numFunctionsNoPlots > 0">
        <table>
          <tr>
            <th>Functions</th>
          </tr>
          <xsl:for-each select="$docs">
            <xsl:if test="count(document(.)[not(qcCompareExprResult/
                          compare/node())]) > 0">
              <tr>
                <td>
                  <a href="{r:logToHTML(string(.))}">
                    <xsl:value-of select="r:getCompareExprName(string(.))"/>
                  </a>
                </td>
              </tr>
            </xsl:if>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
      <p>All functions had plots</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Warnings/Errors -->
    <h2><a name="warnsErrors">Warnings/Errors Comparisons</a></h2>    
    <xsl:choose>
      <xsl:when test="count(document($docs)/qcCompareExprResult/
                            compare[testWarnings|controlWarnings|
                                    testError|controlError]) > 0">
        <table>
          <tr>
            <th></th>
            <th>Functions</th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
          </tr>
          <xsl:for-each select="document($docs)[qcCompareExprResult/
                                compare[testWarnings|controlWarnings]]">
            <xsl:variable name="docPosition" select="position()"/>
            <xsl:for-each select="./qcCompareExprResult/
                                  compare[testWarnings|controlWarnings]">
              <tr>
                <xsl:if test="position()=1 and $docPosition=1">
                  <th rowspan="{count(document($docs)/qcCompareExprResult/
                               compare[testWarnings|controlWarnings])}"
                      class="topAlign">
                    Warnings
                  </th>
                </xsl:if>
                <xsl:if test="position()=1">
                  <td rowspan="{last()}" class="topAlign">
                    <a href="{r:logToHTML(string(../info/logFilename))}">
                      <xsl:value-of select="r:getCompareExprName(
                                            string(../info/logFilename))"/>
                    </a>
                  </td>
                </xsl:if>
                <td>
                  <xsl:value-of select="@type"/>
                </td>
                <td class="topAlign">
                  <xsl:apply-templates select="testWarnings"/>
                </td>
                <td class="topAlign">
                  <xsl:apply-templates select="controlWarnings"/>
                </td>
              </tr>
            </xsl:for-each>
          </xsl:for-each>
          <xsl:for-each select="document($docs)[qcCompareExprResult/
                                compare[testError|controError]]">
            <xsl:variable name="docPosition" select="position()"/>
            <xsl:for-each select="./qcCompareExprResult/
                                  compare[testError|controlError]">
              <tr>
                <xsl:if test="position()=1 and $docPosition=1">
                  <th rowspan="{count(document($docs)/qcCompareExprResult/
                               compare[testError|controlError])}"
                      class="topAlign">
                    Errors
                  </th>
                </xsl:if>
                <xsl:if test="position()=1">
                  <td rowspan="{last()}" class="topAlign">
                    <a href="{r:logToHTML(string(../info/logFilename))}">
                      <xsl:value-of select="r:getCompareExprName(
                                            string(../info/logFilename))"/>
                    </a>
                  </td>
                </xsl:if>
                <td>
                  <xsl:value-of select="@type"/>
                </td>
                <td class="topAlign">
                  <xsl:value-of select="testError"/>
                </td>
                <td class="topAlign">
                  <xsl:value-of select="controlError"/>
                </td>
              </tr>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No differences in warnings/errors were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Unpaired -->
    <h2><a name="unpaired">Unpaired</a></h2>
    <xsl:choose>
      <xsl:when test="document($docs)/qcCompareExprResult/unpaired/test/node()
               | document($docs)/qcCompareExprResult/unpaired/control/node()">
      <table>
        <tr>
          <th></th>
          <th>Files</th>
          <th>Format</th>
          <th>Test</th>
          <th>Control</th>
        </tr>
        <xsl:if test="document($docs)/qcCompareExprResult/unpaired/*/*/plot">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Plots</xsl:with-param>
            <xsl:with-param name="which">plot</xsl:with-param>
            <xsl:with-param name="docs" select="$docs"/>
          </xsl:call-template>
        </xsl:if>
        <xsl:if test="document($docs)/qcCompareExprResult/unpaired/*/*/
                      warnings">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Warnings</xsl:with-param>
            <xsl:with-param name="which">warnings</xsl:with-param>
            <xsl:with-param name="docs" select="$docs"/>
          </xsl:call-template>
        </xsl:if>
        <xsl:if test="document($docs)/qcCompareExprResult/unpaired/*/*/error">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Errors</xsl:with-param>
            <xsl:with-param name="which">error</xsl:with-param>
            <xsl:with-param name="docs" select="$docs"/>
          </xsl:call-template>
        </xsl:if>
      </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No files were unpaired.</p>
      </xsl:otherwise>
    </xsl:choose>
  </body>
  </html>
</xsl:template>

<!-- Template for qcCompareFileResult -->
<xsl:template match="/qcCompareFileResult">
  <xsl:variable name="docs" select="qcCompareExprResult"/>
  <xsl:variable name="numDiffFiles"
                select="count(document($docs)/qcCompareExprResult[compare/
                        comparison[result='different']])"/>
  <xsl:variable name="numDiffPlots"
                select="count(document($docs)/qcCompareExprResult/compare/
                        comparison[result='different'])"/>
  <xsl:variable name="numIdenticalFiles"
                select="count(document($docs)/qcCompareExprResult[compare/
                        comparison[result='identical']])"/>
  <xsl:variable name="numIdenticalPlots"
                select="count(document($docs)/qcCompareExprResult/compare/
                        comparison[result='identical'])"/>
  <xsl:variable name="numFilesNoPlots"
                select="count(document($docs)[not(qcCompareExprResult/
                        compare/node())])"/>
  <html>
  <head>
    <title>Compare File Result</title>
    <style type="text/css">
      table {border: 1px solid #585858;
             background-color: #D8D8D8;}
      th {background-color: #80add6;
          font-weight: bold;
          text-align: left;
          padding-left: 5px;
          padding-right: 5px;
          padding-top: 3px;
          padding-bottom: 3px;}
      td {background-color: #fdfad9;
          padding-left: 5px;
          padding-right: 5px;
          padding-top: 3px;
          padding-bottom: 3px;}
    </style>  
  </head>
  <body>
    <h1>Compare File Result</h1>
    <!-- Table of contents -->
    <ol>
      <li>
        <a href="#info">Info</a>
      </li>
      <li>
        <a href="#plotComp">
          Plot Comparisons
        </a>
        <ul>
          <li>
            <a href="#diffPlots">Different Plots</a>
            <xsl:choose>
              <xsl:when test="$numDiffPlots = 0">
                [none]
              </xsl:when>
              <xsl:otherwise>
                [<xsl:value-of select="$numDiffPlots"/> in
                 <xsl:value-of select="$numDiffFiles"/>
                 file<xsl:if test="$numDiffFiles > 1">s</xsl:if>]
              </xsl:otherwise>
            </xsl:choose>
          </li>
          <li>
            <a href="#identicalPlots">Identical Plots</a>
            <xsl:choose>
              <xsl:when test="$numIdenticalPlots = 0">
                [none]
              </xsl:when>
              <xsl:otherwise>
                [<xsl:value-of select="$numIdenticalPlots"/> in
                 <xsl:value-of select="$numIdenticalFiles"/>
                 file<xsl:if test="$numIdenticalFiles > 1">s</xsl:if>]
              </xsl:otherwise>
            </xsl:choose>
          </li>
          <li>
            <a href="#noPlots">No plots to compare</a>
            <xsl:choose>
              <xsl:when test="$numFilesNoPlots = 0">
                [none]
              </xsl:when>
              <xsl:otherwise>
                [<xsl:value-of select="$numFilesNoPlots"/>
                file<xsl:if test="$numFilesNoPlots > 1">s</xsl:if>]
              </xsl:otherwise>
            </xsl:choose>
          </li>
        </ul>
      </li>
      <li><a href="#warnsErrors">Warnings/Errors Comparisons</a></li>
      <li><a href="#unpaired">Unpaired</a></li>
    </ol>
    <!-- Info -->
    <h2><a name="info">Info</a></h2>
    <table>
      <tr>
        <th></th>
        <th style="text-align: center">Test</th>
        <th style="text-align: center">Control</th>
        <th style="text-align: center">Comparison</th>
      </tr>
      <tr>
        <th>Version</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFileResult/info/Rver"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFileResult/info/Rver"/>
        <xsl:apply-templates select="info/Rver"/>
      </tr>
      <tr>
        <th>OS</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFileResult/info/OS"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFileResult/info/OS"/>
        <xsl:apply-templates select="info/OS"/>
      </tr>
      <tr>
        <th>Date</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFileResult/info/date"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFileResult/info/date"/>
        <xsl:apply-templates select="info/date"/>
      </tr>
      <tr>
        <th>Call</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFileResult/info/call"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFileResult/info/call"/>
        <xsl:apply-templates select="info/call"/>
      </tr>
      <tr>
        <th>Directory</th>
        <xsl:apply-templates select=
          "document(info/testLog)/qcPlotFileResult/info/directory"/>
        <xsl:apply-templates select=
          "document(info/controlLog)/qcPlotFileResult/info/directory"/>
        <xsl:apply-templates select="info/path"/>
      </tr>
      <tr>
        <th>Log Filename</th>
        <td>
          <a href="{r:logToHTML(string(info/testLog))}">
            <!-- Either basename of this testLog, or go through the log to
                 it's own info -->
            <xsl:value-of select="r:call('basename', string(info/testLog))"/>
          </a>
        </td>
        <td>
          <a href="{r:logToHTML(string(info/controlLog))}">
            <!-- Either basename of this testLog, or go through the log to
                 it's own info -->
            <xsl:value-of select="r:call('basename',
                                         string(info/controlLog))"/>
          </a>
        </td>
        <td>
          <xsl:value-of select="info/logFilename"/>
        </td>
      </tr>
    </table>
    <!-- Plots -->
    <h2><a name="plotComp">Plot Comparisons</a></h2>
    <!-- Different Plots -->
    <h3><a name="diffPlots">Different plots</a></h3>
    <xsl:choose>
      <xsl:when test="$numDiffFiles > 0">
        <table>
          <tr>
            <th>Files</th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
            <th>Diff output</th>
            <th>Plot of differences</th>
          </tr>
          <xsl:for-each select="$docs">
            <xsl:variable name="doc" select="."/>
            <xsl:for-each select="document(.)/qcCompareExprResult/
                                  compare[comparison[result='different']]">
            <xsl:variable name="comparePosition" select="position()"/>
              <xsl:for-each select="comparison[result='different']">
                <tr>
                  <xsl:if test="$comparePosition=1 and position()=1">
                    <td rowspan="{count(../../compare/
                                 comparison[result='different'])}">
                      <a href="{r:logToHTML(string($doc))}">
                        <xsl:value-of select="r:getCompareExprName(
                                              string($doc))"/>
                      </a>
                    </td>
                  </xsl:if>
                  <xsl:choose>
                    <xsl:when test="position()=1">
                      <td rowspan="{last()}">
                        <xsl:value-of select="../@type"/>
                      </td>
                      <xsl:apply-templates select="."/>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffFile))}">
                          <xsl:value-of select="diffFile"/>
                        </a>
                      </td>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffPlot))}">
                          <xsl:value-of select="diffPlot"/>
                        </a>
                      </td>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:apply-templates select="."/>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffFile))}">
                          <xsl:value-of select="diffFile"/>
                        </a>
                      </td>
                      <td>
                        <a href="{r:call('file.path',
                                  string(../../info/path),
                                  string(diffPlot))}">
                          <xsl:value-of select="diffPlot"/>
                        </a>
                      </td>
                    </xsl:otherwise>
                  </xsl:choose>
                </tr>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No differences were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Identical plots -->
    <h3><a name="identicalPlots">Identical plots</a></h3>
    <xsl:choose>
      <xsl:when test="$numIdenticalFiles > 0">
        <table>
          <tr>
            <th>Files</th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
          </tr>
          <xsl:for-each select="$docs">
            <xsl:variable name="doc" select="."/>
            <xsl:for-each select="document(.)/qcCompareExprResult/
                                  compare[comparison[result='identical']]">
            <xsl:variable name="comparePosition" select="position()"/>
              <xsl:for-each select="comparison[result='identical']">
                <tr>
                <xsl:if test="$comparePosition=1 and position()=1">
                  <td rowspan="{count(../../compare/
                                comparison[result='identical'])}">
                    <a href="{r:logToHTML(string($doc))}">
                      <xsl:value-of select="r:getCompareExprName(string(
                                            $doc))"/>
                    </a>
                  </td>
                </xsl:if>
                <xsl:choose>
                  <xsl:when test="position()=1">
                    <td rowspan="{last()}">
                      <xsl:value-of select="../@type"/>
                    </td>
                    <xsl:apply-templates select="."/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates select="."/>
                  </xsl:otherwise>
                </xsl:choose>
                </tr>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No identical plots were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- No plots to compare-->
    <h3><a name="noPlots">No plots to compare</a></h3>
    <xsl:choose>
      <xsl:when test="$numFilesNoPlots > 0">
        <table>
          <tr>
            <th>Files</th>
          </tr>
          <xsl:for-each select="$docs">
            <xsl:if test="count(document(.)[not(qcCompareExprResult/
                          compare/node())]) > 0">
              <tr>
                <td>
                  <a href="{r:logToHTML(string(.))}">
                    <xsl:value-of select="r:getCompareExprName(string(.))"/>
                  </a>
                </td>
              </tr>
            </xsl:if>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
      <p>All files had plots</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Warnings/Errors -->
    <h2><a name="warnsErrors">Warnings/Errors Comparisons</a></h2>    
    <xsl:choose>
      <xsl:when test="count(document($docs)/qcCompareExprResult/
                            compare[testWarnings|controlWarnings|
                                    testError|controlError]) > 0">
        <table>
          <tr>
            <th></th>
            <th>Files</th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
          </tr>
          <xsl:for-each select="document($docs)[qcCompareExprResult/
                                compare[testWarnings|controlWarnings]]">
            <xsl:variable name="docPosition" select="position()"/>
            <xsl:for-each select="./qcCompareExprResult/
                                  compare[testWarnings|controlWarnings]">
              <tr>
                <xsl:if test="position()=1 and $docPosition=1">
                  <th rowspan="{count(document($docs)/qcCompareExprResult/
                               compare[testWarnings|controlWarnings])}"
                      class="topAlign">
                    Warnings
                  </th>
                </xsl:if>
                <xsl:if test="position()=1">
                  <td rowspan="{last()}" class="topAlign">
                    <a href="{r:logToHTML(string(../info/logFilename))}">
                      <xsl:value-of select="r:getCompareExprName(
                                            string(../info/logFilename))"/>
                    </a>
                  </td>
                </xsl:if>
                <td>
                  <xsl:value-of select="@type"/>
                </td>
                <td class="topAlign">
                  <xsl:apply-templates select="testWarnings"/>
                </td>
                <td class="topAlign">
                  <xsl:apply-templates select="controlWarnings"/>
                </td>
              </tr>
            </xsl:for-each>
          </xsl:for-each>
          <xsl:for-each select="document($docs)[qcCompareExprResult/
                                compare[testError|controError]]">
            <xsl:variable name="docPosition" select="position()"/>
            <xsl:for-each select="./qcCompareExprResult/
                                  compare[testError|controlError]">
              <tr>
                <xsl:if test="position()=1 and $docPosition=1">
                  <th rowspan="{count(document($docs)/qcCompareExprResult/
                               compare[testError|controlError])}"
                      class="topAlign">
                    Errors
                  </th>
                </xsl:if>
                <xsl:if test="position()=1">
                  <td rowspan="{last()}" class="topAlign">
                    <a href="{r:logToHTML(string(../info/logFilename))}">
                      <xsl:value-of select="r:getCompareExprName(
                                            string(../info/logFilename))"/>
                    </a>
                  </td>
                </xsl:if>
                <td>
                  <xsl:value-of select="@type"/>
                </td>
                <td class="topAlign">
                  <xsl:value-of select="testError"/>
                </td>
                <td class="topAlign">
                  <xsl:value-of select="controlError"/>
                </td>
              </tr>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No differences in warnings/errors were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Unpaired -->
    <h2><a name="unpaired">Unpaired</a></h2>
    <xsl:choose>
      <xsl:when test="document($docs)/qcCompareExprResult/unpaired/test/node()
               | document($docs)/qcCompareExprResult/unpaired/control/node()">
      <table>
        <tr>
          <th></th>
          <th>Files</th>
          <th>Format</th>
          <th>Test</th>
          <th>Control</th>
        </tr>
        <xsl:if test="document($docs)/qcCompareExprResult/unpaired/*/*/plot">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Plots</xsl:with-param>
            <xsl:with-param name="which">plot</xsl:with-param>
            <xsl:with-param name="docs" select="$docs"/>
          </xsl:call-template>
        </xsl:if>
        <xsl:if test="document($docs)/qcCompareExprResult/unpaired/*/*/
                      warnings">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Warnings</xsl:with-param>
            <xsl:with-param name="which">warnings</xsl:with-param>
            <xsl:with-param name="docs" select="$docs"/>
          </xsl:call-template>
        </xsl:if>
        <xsl:if test="document($docs)/qcCompareExprResult/unpaired/*/*/error">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Errors</xsl:with-param>
            <xsl:with-param name="which">error</xsl:with-param>
            <xsl:with-param name="docs" select="$docs"/>
          </xsl:call-template>
        </xsl:if>
      </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No files were unpaired.</p>
      </xsl:otherwise>
    </xsl:choose>
  </body>
  </html>
</xsl:template>

<!-- Other templates -->

<xsl:template match="Rver | OS | date | directory | path">
  <td><xsl:value-of select="."/></td>
</xsl:template>

<xsl:template match="call">
  <td style="font-family: monospace"><xsl:value-of select="."/></td>
</xsl:template>

<xsl:template match="comparison">
  <td>
    <a href="{@testFile}">
      <xsl:value-of select="r:call('basename', string(@testFile))"/>
    </a>
  </td>
  <td>
    <a href="{@controlFile}">
      <xsl:value-of select="r:call('basename', string(@controlFile))"/>
    </a>
  </td>
</xsl:template>

<xsl:template match="controlWarnings | testWarnings">
  <xsl:value-of select="."/>
  <br/>
</xsl:template>

<xsl:template name="unpaired">
  <xsl:param name="title"/>
  <xsl:param name="which"/>
  <xsl:param name="docs"/>
  <xsl:for-each select="document($docs)[qcCompareExprResult/unpaired/*/*/
                        *[name() = $which]]">
    <xsl:variable name="docPosition" select="position()"/>
    <xsl:for-each select="./qcCompareExprResult/unpaired/*/
                          *[*[name() = $which]]">
      <tr>
        <xsl:if test="position()=1 and $docPosition=1">
          <th rowspan="{count(document($docs)/qcCompareExprResult/unpaired/*
                        /*[*[name() = $which]])}"
              class="topAlign">
            <xsl:value-of select="$title"/>
          </th>
        </xsl:if>
        <xsl:if test="position()=1">
          <td rowspan="{last()}" class="topAlign">
            <a href="{r:logToHTML(string(../../../info/logFilename))}">
              <xsl:value-of select="r:getCompareExprName(
                                    string(../../../info/logFilename))"/>
            </a>
          </td>
        </xsl:if>
        <xsl:variable name="format" select="local-name()"/>
        <td>
          <xsl:value-of select="$format"/>
        </td>
        <td>
          <xsl:for-each select="../../test/*[name() = $format]/*[name() =
                                $which]">
            <xsl:choose>
              <xsl:when test="'plot'=$which">
                <a href="{r:call('file.path', string(../../../../info/
                         testDirectory), string(.))}">
                  <xsl:value-of select="."/>
                </a>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="."/>
              </xsl:otherwise>
            </xsl:choose>
            <br/>
          </xsl:for-each>
        </td>
        <td>
          <xsl:for-each select="../../control/*[name() = $format]/*[name() =
                                $which]">
            <xsl:choose>
              <xsl:when test="'plot'=$which">
                <a href="{r:call('file.path', string(../../../../info/
                         controlDirectory), string(.))}">
                  <xsl:value-of select="."/>
                </a>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="."/>
              </xsl:otherwise>
            </xsl:choose>
            <br/>
          </xsl:for-each>
        </td>
      </tr>
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
