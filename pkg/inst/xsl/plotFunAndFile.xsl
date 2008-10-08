<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:r="http://www.r-project.org"
                exclude-result-prefixes="r"
                extension-element-prefixes="r">
<xsl:output method="html" indent="yes"/>

<!-- Template for qcPlotFunResult -->
<xsl:template match="/qcPlotFunResult">
  <html>
  <head>
    <title>Plot Function Result</title>
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
    <h2>Plot Function Result</h2>
    <!-- Info Section -->
    <table>
    <tr>
      <th>R Version</th>
      <td><xsl:value-of select="info/Rver"/></td>
    </tr>
    <tr>
      <th>OS</th>
      <td><xsl:value-of select="info/OS"/></td>
    </tr>
    <tr>
      <th>Date</th>
      <td><xsl:value-of select="info/date"/></td>
    </tr>
    <tr>
      <th>Call</th>
      <td style="font-family: monospace">
        <xsl:value-of select="info/call"/>
      </td>
    </tr>
    <tr>
      <th>Directory</th>
      <td><xsl:value-of select="info/directory"/></td>
    </tr>
    <tr>
      <th>Log Filename</th>
      <td><xsl:value-of select="info/logFilename"/></td>
    </tr>
    </table>
    <br/>
    <!-- Listing of functions -->
    <h3>Functions</h3>
    <ul>
      <xsl:for-each select="qcPlotExprResult">
        <li>
          <a href="#{r:call('basename', string(.))}">
            <xsl:value-of select="r:call('basename', string(.))"/>
          </a>
        </li>
      </xsl:for-each>
    </ul>
    <!-- Plots section -->
    <h3>Plots</h3>
    <xsl:for-each select="document(qcPlotExprResult)/qcPlotExprResult">
      <a name="{info/logFilename}" href="{r:logToHTML(string(info/directory),
                                          string(info/logFilename))}">
        <xsl:value-of select="info/logFilename"/>
      </a>
      <br/>
      <table>
      <tr>
        <th>Format</th>
        <th>Plots</th>
        <xsl:if test="normalize-space(plots/warnings)">
          <th>Warnings</th>
        </xsl:if>
        <xsl:if test="normalize-space(plots/error)">
          <th>Error</th>
        </xsl:if>
      </tr>
      <xsl:for-each select="plots">
        <xsl:variable name="format" select="@type"/>
        <xsl:for-each select="plot">
          <tr>
            <xsl:if test="position()=1">
              <td rowspan="{last()}" class="topAlign">
                <xsl:value-of select="$format"/>
              </td>
            </xsl:if>
            <td>
              <a href="{r:call('file.path', string(../../info/directory),
                        string(.))}">
                <xsl:value-of select="."/>
              </a>
            </td>
            <xsl:if test="position()=1">
              <xsl:if test="normalize-space(../warnings)">
                <td rowspan="{last()}" class="topAlign">
                  <xsl:for-each select="../warnings">
                    <xsl:value-of select="."/>
                    <xsl:if test="position()!=last()">
                      <br/>
                    </xsl:if>
                  </xsl:for-each>
                </td>
              </xsl:if>
              <xsl:if test="normalize-space(../error)">
                <td rowspan="{last()}" class="topAlign">
                  <xsl:value-of select="../error"/>
                </td>
              </xsl:if>
            </xsl:if>
          </tr>
        </xsl:for-each>
      </xsl:for-each>
      </table>
      <br/><br/>
    </xsl:for-each>
  </body>
  </html>
</xsl:template>

<!-- Template for qcPlotFileResult -->
<xsl:template match="/qcPlotFileResult">
  <html>
  <head>
    <title>Plot File Result</title>
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
    <h2>Plot File Result</h2>
    <!-- Info Section -->
    <table>
    <tr>
      <th>R Version</th>
      <td><xsl:value-of select="info/Rver"/></td>
    </tr>
    <tr>
      <th>OS</th>
      <td><xsl:value-of select="info/OS"/></td>
    </tr>
    <tr>
      <th>Date</th>
      <td><xsl:value-of select="info/date"/></td>
    </tr>
    <tr>
      <th>Call</th>
      <td style="font-family: monospace">
        <xsl:value-of select="info/call"/>
      </td>
    </tr>
    <tr>
      <th>Directory</th>
      <td><xsl:value-of select="info/directory"/></td>
    </tr>
    <tr>
      <th>Log Filename</th>
      <td><xsl:value-of select="info/logFilename"/></td>
    </tr>
    </table>
    <br/>
    <!-- Listing of files -->
    <h3>Files</h3>
    <ul>
      <xsl:for-each select="qcPlotExprResult">
        <li>
          <a href="#{r:call('basename', string(.))}">
            <xsl:value-of select="r:call('basename', string(.))"/>
          </a>
        </li>
      </xsl:for-each>
    </ul>
    <!-- Plots section -->
    <h3>Plots</h3>
    <xsl:for-each select="document(qcPlotExprResult)/qcPlotExprResult">
      <a name="{info/logFilename}" href="{r:logToHTML(string(info/directory),
                                          string(info/logFilename))}">
        <xsl:value-of select="info/logFilename"/>
      </a>
      <br/>
      <table>
      <tr>
        <th>Format</th>
        <th>Plots</th>
        <xsl:if test="normalize-space(plots/warnings)">
          <th>Warnings</th>
        </xsl:if>
        <xsl:if test="normalize-space(plots/error)">
          <th>Error</th>
        </xsl:if>
      </tr>
      <xsl:for-each select="plots">
        <xsl:variable name="format" select="@type"/>
        <xsl:for-each select="plot">
          <tr>
            <xsl:if test="position()=1">
              <td rowspan="{last()}" class="topAlign">
                <xsl:value-of select="$format"/>
              </td>
            </xsl:if>
            <td>
              <a href="{r:call('file.path', string(../../info/directory),
                        string(.))}">
                <xsl:value-of select="."/>
              </a>
            </td>
            <xsl:if test="position()=1">
              <xsl:if test="normalize-space(../warnings)">
                <td rowspan="{last()}" class="topAlign">
                  <xsl:for-each select="../warnings">
                    <xsl:value-of select="."/>
                    <xsl:if test="position()!=last()">
                      <br/>
                    </xsl:if>
                  </xsl:for-each>
                </td>
              </xsl:if>
              <xsl:if test="normalize-space(../error)">
                <td rowspan="{last()}" class="topAlign">
                  <xsl:value-of select="../error"/>
                </td>
              </xsl:if>
            </xsl:if>
          </tr>
        </xsl:for-each>
      </xsl:for-each>
      </table>
      <br/><br/>
    </xsl:for-each>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>
