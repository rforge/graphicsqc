<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:r="http://www.r-project.org"
                exclude-result-prefixes="r"
                extension-element-prefixes="r">
<xsl:output method="html" indent="yes"/>

<xsl:template match="/qcCompareExprResult">
  <html>
  <head>
    <title>Compare Expression Result</title>
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
    <h1>Compare Expression Result</h1>
    <!-- Table of contents -->
    <ol>
      <li><a href="#info">Info</a></li>
      <li><a href="#plotComp">Plot Comparisons</a></li>
      <li><a href="#warnsErrors">Warnings/Errors Comparisons</a></li>
      <li><a href="#unpaired">Unpaired</a></li>
    </ol>
    <h2><a name="info">Info</a></h2>
    <!-- Info -->
    <table>
      <tr>
        <th></th>
        <th style="text-align: center">Test</th>
        <th style="text-align: center">Control</th>
        <th style="text-align: center">Comparison</th>
      </tr>
      <tr>
        <th>Version</th>
        <xsl:apply-templates select="testInfo/Rver | controlInfo/Rver"/>
        <xsl:apply-templates select="info/Rver"/>
      </tr>
      <tr>
        <th>OS</th>
        <xsl:apply-templates select="testInfo/OS | controlInfo/OS"/>
        <xsl:apply-templates select="info/OS"/>
      </tr>
      <tr>
        <th>Date</th>
        <xsl:apply-templates select="testInfo/date | controlInfo/date"/>
        <xsl:apply-templates select="info/date"/>
      </tr>
      <tr>
        <th>Call</th>
        <xsl:apply-templates select="testInfo/call | controlInfo/call"/>
        <xsl:apply-templates select="info/call"/>
      </tr>
      <tr>
        <th>Directory</th>
        <xsl:apply-templates select="testInfo/directory |
                                     controlInfo/directory"/>
        <xsl:apply-templates select="info/path"/>
      </tr>
      <tr>
        <th>Log Filename</th>
        <td>
          <a href="{r:logToHTML(string(testInfo/directory),
                           string(testInfo/logFilename))}">
            <xsl:value-of select="testInfo/logFilename"/>
          </a>
        </td>
        <td>
          <a href="{r:logToHTML(string(controlInfo/directory),
                           string(controlInfo/logFilename))}">
            <xsl:value-of select="controlInfo/logFilename"/>
          </a>
        </td>
        <td>
          <!-- or anchor over document('') -->
          <xsl:value-of select="info/logFilename"/>
        </td>
      </tr>
    </table>
    <br/>
    <!-- Plots -->
    <h2><a name="plotComp">Plot Comparisons</a></h2>
    <h3>Different plots</h3>
    <xsl:choose>
      <xsl:when test="compare/*/result='different'">
        <table>
          <tr>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
            <th>Diff output</th>
            <th>Plot of differences</th>
          </tr>
          <xsl:for-each select="compare[comparison[result='different']]">
            <xsl:for-each select="comparison[result='different']">
              <tr>
              <xsl:choose>
                <xsl:when test="position()=1">
                  <td rowspan="{last()}" class="topAlign">
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
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No differences were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    
    <h3>Identical plots</h3>
    <xsl:choose>
      <xsl:when test="compare/*/result='identical'">
        <table>
          <tr>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
          </tr>
          <xsl:for-each select="compare[comparison[result='identical']]">
            <xsl:for-each select="comparison[result='identical']">
              <tr>
              <xsl:choose>
                <xsl:when test="position()=1">
                  <td rowspan="{last()}" class="topAlign">
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
        </table>
      </xsl:when>
      <xsl:otherwise>
        <p>No identical plots were found.</p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Warnings/errors -->
    <h2><a name="warnsErrors">Warnings/Errors Comparisons</a></h2>
    <xsl:choose>
      <xsl:when test="compare/controlWarnings | compare/testWarnings |
                      compare/controlError | compare/testError">
        <table>
          <tr>
            <th></th>
            <th>Format</th>
            <th>Test</th>
            <th>Control</th>
          </tr>
          <xsl:for-each select="compare[controlWarnings] |
                                compare[testWarnings]">
            <tr>
              <xsl:if test="position()=1">
                <th rowspan="{last()}" class="topAlign">
                  Warnings
                </th>
              </xsl:if>
              <td>
                <xsl:value-of select="@type"/>
              </td>
              <td style="vertical-align: top">
                <xsl:apply-templates select="testWarnings"/>
              </td>
              <td style="vertical-align: top">
                <xsl:apply-templates select="controlWarnings"/>
              </td>
            </tr>
          </xsl:for-each>
          <xsl:for-each select="compare[controlError]|compare[testError]">
            <tr>
              <xsl:if test="position()=1">
                <th rowspan="{last()}" style="vertical-align:top">
                  Errors
                </th>
              </xsl:if>
              <td>
                <xsl:value-of select="@type"/>
              </td>
              <td style="vertical-align: top">
                <xsl:value-of select="testError"/>
              </td>
              <td style="vertical-align: top">
                <xsl:value-of select="controlError"/>
              </td>
            </tr>
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
      <xsl:when test="unpaired/test/node() | unpaired/control/node()">
      <table>
        <tr>
          <th></th>
          <th>Format</th>
          <th>Test</th>
          <th>Control</th>
        </tr>
        <xsl:if test="unpaired/*/*/plot">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Plots</xsl:with-param>
            <xsl:with-param name="which">plot</xsl:with-param>
          </xsl:call-template>
        </xsl:if>
        <xsl:if test="unpaired/*/*/warnings">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Warnings</xsl:with-param>
            <xsl:with-param name="which">warnings</xsl:with-param>
          </xsl:call-template>
        </xsl:if>
        <xsl:if test="unpaired/*/*/error">
          <xsl:call-template name="unpaired">
            <xsl:with-param name="title">Errors</xsl:with-param>
            <xsl:with-param name="which">error</xsl:with-param>
          </xsl:call-template>
        </xsl:if>
      </table>
      </xsl:when>
      <xsl:otherwise>
        <p>Nothing was unpaired.</p>
      </xsl:otherwise>
    </xsl:choose>
  </body>
  </html>
</xsl:template>

<!-- Templates -->
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
  <xsl:for-each select="unpaired/*/*[*[name() = $which]]">
    <tr>
      <xsl:if test="position()=1">
        <th rowspan="{last()}" class="topAlign">
          <xsl:value-of select="$title"/>
        </th>
      </xsl:if>
      <xsl:variable name="format" select="local-name()"/>
      <td>
        <xsl:value-of select="$format"/>
      </td>
      <td>
        <xsl:for-each select="../../test/*[name() = $format]/*[name() =
                              $which]">
          <xsl:value-of select="."/>
          <br/>
        </xsl:for-each>
      </td>
      <td>
        <xsl:for-each select="../../control/*[name() = $format]/*[name() =
                              $which]">
          <xsl:value-of select="."/>
          <br/>
        </xsl:for-each>
      </td>
    </tr>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
