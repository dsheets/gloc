<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/TR/xhtml1/strict">

  <xsl:output method="html" />

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="footer" name="footer">
    <div id="footer">
      <div id="footerlinks">
        <p id="gloc_foot">
          <a href="/gloc/">gloc documentation</a><br />
          <a href="/gloc/glocode/">gloc online development environment</a><br />
          <a href="https://github.com/ashima/gloc">Source on Github
          (BSD-3-Clause)</a>
        </p>
        <p id="glol_foot">
          <a href="/gloc/test/webgl/glol/">glol tests</a><br />
          <a href="/gloc/glol.js">glol.js</a>
        </p>
        <p id="contact_foot">
          <a href="https://github.com/ashima/gloc/issues">Report a
          problem</a><br />
          <a href="mailto:sheets@ashimaarts.com">E-mail the developer,
          David Sheets</a><br />
          <a href="mailto:info@ashimaarts.com">E-mail the company,
          Ashima Arts</a>
        </p>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="doc">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>gloc links our creativity! carpe ignem!</title>
        <link rel="stylesheet" href="gloc.css" type="text/css"></link>
      </head>
      <body>
        <xsl:apply-templates />
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
