<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/TR/xhtml1/strict">

  <xsl:output method="html" />

  <xsl:template name="flag">
    <td class="flag"><xsl:value-of select="@flag" /></td>
  </xsl:template>

  <xsl:template name="descr">
    <td class="descr"><xsl:value-of select="@descr" /></td>
  </xsl:template>

  <xsl:template match="alt">
    <xsl:param name="name" />
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
	  <xsl:choose>
	    <xsl:when test="position()=1">
	      <input type="radio" name="{$name}" value="{@flag}">
		<xsl:attribute name="checked" />
	      </input>
	    </xsl:when>
	    <xsl:otherwise>
	      <input type="radio" name="{$name}" value="{@flag}"/>
	    </xsl:otherwise>
	  </xsl:choose>
      </td>
      <xsl:call-template name="descr" />
    </tr>
  </xsl:template>

  <xsl:template match="group">
    <xsl:apply-templates select="alt">
      <xsl:with-param name="name" select="@name" />
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="filelist">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
	<input type="text" class="filelist" name="filelist{@flag}"/>
      </td>
      <xsl:call-template name="descr" />
    </tr>
  </xsl:template>

  <xsl:template match="setlist">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
	<input type="text" class="setlist" name="setlist{@flag}"/>
      </td>
      <xsl:call-template name="descr" />
    </tr>
  </xsl:template>

  <xsl:template match="filename">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
	<input type="text" class="filename" name="filename{@flag}"/>
      </td>
      <xsl:call-template name="descr" />
    </tr>
  </xsl:template>

  <xsl:template match="choice">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
	<select name="choice{@flag}">
	  <xsl:for-each select="alt">
	    <option><xsl:value-of select="@name" /></option>
	  </xsl:for-each>
	</select>
      </td>
      <xsl:call-template name="descr" />
    </tr>
  </xsl:template>

  <xsl:template match="option">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
	<input type="checkbox" name="option{@flag}" value="{@flag}" />
      </td>
      <xsl:call-template name="descr" />
    </tr>
  </xsl:template>

  <xsl:template match="cli">
    <html>
      <head>
	<title>gloc links our creativity! carpe ignem!</title>
	<script src="gloc_platform_js.js"></script>
	<script src="_build/gloc_js.d.js"></script>
	<style type="text/css">
	  .flag, .input { text-align: right }
	</style>
      </head>
      <body>
	<h1>gloc <xsl:value-of select="@version"/>
	by <a href="http://ashimagroup.net/"><xsl:value-of select="@distributor"/></a>
	</h1>
	<form name="gloc">
	  <table>
	    <xsl:apply-templates />
	    <tr><td />
	    <td style="text-align: center"><input type="submit" value="gloc"/>
	    </td><td /></tr>
	  </table>
	</form>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
