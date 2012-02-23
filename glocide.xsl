<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/TR/xhtml1/strict">

  <xsl:output method="html" />

  <xsl:template name="glol_params">
    <h2>glol</h2>
    <form>
      <ul>
        <li><input type="text" /></li>
      </ul>
    </form>
  </xsl:template>

  <xsl:template name="glom_name">
    <xsl:if test="@name">
      <h3><xsl:value-of select="@name" /></h3>
    </xsl:if>
  </xsl:template>

  <xsl:template name="glo_name">
    <xsl:choose>
      <xsl:when test="@name"><xsl:value-of select="@name" /></xsl:when>
      <xsl:otherwise>[unknown]</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <title>glosse : glo shader system editor</title>
        <meta charset="utf-8" />
        <meta http-equiv="X-UA-Compatible" content="IE=Edge,chrome=1" />

        <link rel="stylesheet" type="text/css" href="glocide.css" />
        <link rel="stylesheet" type="text/css" href="../CodeMirror2/lib/codemirror.css" />
        <script src="../CodeMirror2/lib/codemirror.js"></script>
        <script src="../CodeMirror2/mode/clike/clike.js"></script>
        <script src="../CodeMirror2/keymap/emacs.js"></script>
      </head>
      <body>
        <xsl:choose>
          <xsl:when test="glom">
            <div id="sidebar">
              <div id="glol">
                <xsl:call-template name="glol_params" />
              </div>
              <div id="glom">
                <h2>glom</h2>
                <xsl:apply-templates mode="glom">
                  <xsl:with-param name="path" />
                </xsl:apply-templates>
              </div>
            </div>
            <div id="gloc">
              <form>
                <textarea id="source" name="source">
                </textarea>
              </form>
              <div id="stderr">
                stderr
              </div>
              <div id="stdout">
                stdout
              </div>
            </div>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="format_error" />
          </xsl:otherwise>
        </xsl:choose>
        <script type="text/javascript">
          var editor = CodeMirror.fromTextArea(document.getElementById("source"), {
            lineNumbers: true,
            mode: "text/x-csrc",
            keyMap: "emacs"
          });
        </script>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="glom" mode="glom">
    <xsl:param name="path" />
    <xsl:call-template name="glom_name" />
    <ul>
      <xsl:for-each select="child::*">
        <li><xsl:apply-templates select="." mode="glom" /></li>
      </xsl:for-each>
    </ul>
  </xsl:template>

  <xsl:template match="glo" mode="glom">
    <a href="#"><xsl:call-template name="glo_name" /></a>
  </xsl:template>
</xsl:stylesheet>
