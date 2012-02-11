<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="html"/>

  <xsl:param name="suite"/>
  <xsl:param name="tests"/>

  <xsl:template name="json_glom_of_xml_glom">
    <xsl:param name="glom" />
    <xsl:for-each select="$glom/glo | $glom/glom">
      ["<xsl:value-of select="@name"/>",
      <xsl:choose>
        <xsl:when test="local-name()='glo'">
          <xsl:value-of select="json" />
        </xsl:when>
        <xsl:otherwise>
          [<xsl:call-template name="json_glom_of_xml_glom">
            <xsl:with-param name="glom" select="." />
          </xsl:call-template>]
        </xsl:otherwise>
      </xsl:choose>],
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="make_test">
    <xsl:param name="thisgroup"/>
    <xsl:param name="tests"/>
    <xsl:if test="string-length($tests) >0">
      <xsl:variable name="test"
		    select="document(substring-before(concat($tests,' '), ' '))" />
      <xsl:variable name="testgroup"
		    select="substring-before($test/glom/name/text(),'.')" />

      <xsl:if test="$testgroup!=$thisgroup">
	<tr class="testgroup" id="testgroup-{$testgroup}">
	  <th><xsl:value-of select="$testgroup" /></th>
	  
	  <td class="posix" />
	  <td class="js_of_ocaml" />
	  <td class="javascript" />
	</tr>
      </xsl:if>

      <tr class="test testgroup-{$testgroup}" id="test-{$test/glom/name/text()}">
	<script type="text/javascript">
	  tests["<xsl:value-of select="$test/glom/name/text()" />"]
	  ={
	    "args": "<xsl:value-of select="$test/glom/args" />",
	    "tout": "<xsl:value-of select="$test/glom/tout" />",
	    "terr": "<xsl:value-of select="$test/glom/terr" />",
	    "glom": [
            <xsl:call-template name="json_glom_of_xml_glom">
              <xsl:with-param name="glom" select="$test/glom" />
            </xsl:call-template>
            ],
	    "posix": {"out": "<xsl:value-of select="$test/glom/out" />",
	              "err":  "<xsl:value-of select="$test/glom/err" />"}
	    };
	</script>
	<th><xsl:value-of select="$test/glom/name/text()" /></th>

	<td class="posix" />
	<td class="js_of_ocaml" />
	<td class="javascript" />

      </tr>

      <xsl:call-template name="make_test">
	<xsl:with-param name="thisgroup" select="$testgroup" />
	<xsl:with-param name="tests" select="substring-after($tests, ' ')"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="suite">
    <xsl:value-of select="$suite"/>
  </xsl:template>

  <xsl:template match="tests">
    <table id="tests">
      <thead><tr><th colspan="4"><xsl:value-of select="$suite"/> Tests</th></tr></thead>
      <tbody>
	<tr><th />
	<th>POSIX</th>
	<th>js_of_ocaml</th>
	<th>JavaScript</th>
	</tr>
	
	<xsl:call-template name="make_test">
	  <xsl:with-param name="tests" select="$tests" />
	</xsl:call-template>
      </tbody>
    </table>
  </xsl:template>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
