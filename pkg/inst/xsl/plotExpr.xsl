<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:r="http://www.r-project.org"
                exclude-result-prefixes="r"
                extension-element-prefixes="r">
<xsl:output method="html" indent="yes"/>

<xsl:template match="/qcPlotExprResult">
  <xsl:if test="function-available('r:init')">
    <xsl:if test="r:init('--silent')" />
  </xsl:if>
  <html>
  <head>
    <title>Plot Expression Result</title>
    <style type="text/css">
      table {border: 1px solid #585858; background-color: #D8D8D8;}
      .tableMain {background-color: #80add6;}
      .tableData {background-color: #fdfad9;}
    </style>  
  </head>
  <body>
    <h2>Plot Expression Result</h2>
    <table>
    <tr>
      <td class="tableMain"><strong>R Version</strong></td>
      <td class="tableData"><xsl:value-of select="info/Rver"/></td>
    </tr>
    <tr>
      <td class="tableMain"><strong>OS</strong></td>
      <td class="tableData"><xsl:value-of select="info/OS"/></td>
    </tr>
    <tr>
      <td class="tableMain"><strong>Date</strong></td>
      <td class="tableData"><xsl:value-of select="info/date"/></td>
    </tr>
    <tr>
      <td class="tableMain"><strong>Call</strong></td>
      <td class="tableData"><xsl:value-of select="info/call"/></td>
    </tr>
    <tr>
      <td class="tableMain"><strong>Directory</strong></td>
      <td class="tableData"><xsl:value-of select="info/directory"/></td>
    </tr>
    <tr>
      <td class="tableMain"><strong>Log Filename</strong></td>
      <td class="tableData"><xsl:value-of select="info/logFilename"/></td>
    </tr>
    </table>
    <br/>
    <table>
    <tr>
      <th align="left" class="tableMain"><em>Format</em></th>
      <th align="left" class="tableMain"><em>Plots</em></th>
      <xsl:if test="normalize-space(plots/warnings)">
        <th align="left" class="tableMain"><em>Warnings</em></th>
      </xsl:if>
      <xsl:if test="normalize-space(plots/error)">
        <th align="left" class="tableMain"><em>Error</em></th>
      </xsl:if>
    </tr>
    <xsl:for-each select="plots">
    <tr>
      <td class="tableData" style="vertical-align: top;">
        <xsl:value-of select="@type"/>
      </td>
      <td class="tableData">
      <xsl:for-each select="plot">
          <a href="{r:call('file.path', string(../../info/directory), string(.))}">
            <xsl:value-of select="."/>
          </a>
          <xsl:if test="position()!=last()">
            <br/>
          </xsl:if>
      </xsl:for-each>
      </td>
      <xsl:if test="normalize-space(warnings)">
        <td class="tableData">
          <xsl:for-each select="warnings">
            <xsl:value-of select="."/>
            <xsl:if test="position()!=last()"><br/></xsl:if>
          </xsl:for-each>
        </td>
      </xsl:if>
      <xsl:if test="normalize-space(error)">
        <td class="tableData">
            <xsl:value-of select="error"/>
        </td>
      </xsl:if>
    </tr>
    </xsl:for-each>
    </table>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>
