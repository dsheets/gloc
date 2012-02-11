<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="name"/>
  <xsl:param name="args"/>
  <xsl:param name="tout"/>
  <xsl:param name="terr"/>
  <xsl:param name="out"/>
  <xsl:param name="err"/>

  <xsl:template match="glom">
    <xsl:copy>
      <xsl:copy-of select="@*|node()" />
      <name><xsl:value-of select="$name" /></name>
      <args><xsl:value-of select="$args" /></args>
      <tout><xsl:value-of select="$tout" /></tout>
      <terr><xsl:value-of select="$terr" /></terr>
      <out><xsl:value-of select="$out" /></out>
      <err><xsl:value-of select="$err" /></err>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
