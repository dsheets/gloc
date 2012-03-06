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
              <xsl:attribute name="onchange">
                update_cmd('<xsl:value-of select="$name" />',
                '<xsl:value-of select="@flag"/>')
              </xsl:attribute>
            </input>
          </xsl:when>
          <xsl:otherwise>
            <input type="radio" name="{$name}" value="{@flag}">
              <xsl:attribute name="onchange">
                update_cmd('<xsl:value-of select="$name" />',
                '<xsl:value-of select="@flag"/>')
              </xsl:attribute>
            </input>
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
    <xsl:variable name="id" select="generate-id()" />
    <tr class="{$id}">
      <xsl:call-template name="flag" />
      <td class="input">
        <input type="text" class="filelist" name="filelist{@flag}" />
      </td>
      <xsl:call-template name="descr" />
    </tr>
    <script type="text/javascript">
      expandable_list("<xsl:value-of select="$id" />",
                      function (e) {
                        var flag = '<xsl:value-of select="@flag" />';
                        var id = '<xsl:value-of select="$id" />';
                        var files = document.getElementsByClassName(id);
                        var flags = [];
                        for (var i = 0; i &lt; files.length; i++) {
                          var f = files[i].getElementsByTagName("input")[0].value;
                          if (f!="") flags[flags.length]=flag+" "+f;
                        }
                        update_cmd(flag,flags.join(" "));
                        update_fs(e);
                      });
    </script>
  </xsl:template>

  <xsl:template match="setlist">
    <xsl:variable name="id" select="generate-id()" />
    <tr class="{$id}">
      <xsl:call-template name="flag" />
      <td class="input">
        <input type="text" class="setlist" name="setlist{@flag}"/>
      </td>
      <xsl:call-template name="descr" />
    </tr>
    <script type="text/javascript">
      expandable_list("<xsl:value-of select="$id" />",
                      function (e) {
                        var flag = '<xsl:value-of select="@flag" />';
                        var id = '<xsl:value-of select="$id" />';
                        var set = document.getElementsByClassName(id);
                        var flags = [];
                        for (var i = 0; i &lt; set.length; i++) {
                          var s = set[i].getElementsByTagName("input")[0].value;
                          if (s!="") flags[flags.length]=flag+" "+s;
                        }
                        update_cmd(flag,flags.join(" "));
                      });
    </script>
  </xsl:template>

  <xsl:template match="filename">
    <xsl:variable name="id" select="generate-id()" />
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
        <input type="text" class="filename" id="{$id}" name="filename{@flag}"/>
      </td>
      <xsl:call-template name="descr" />
    </tr>
    <script type="text/javascript">
      document.getElementById("<xsl:value-of select="$id"/>").onblur =
      function (e) {
        var flag = '<xsl:value-of select="@flag" />';
        if (this.value!="") {
          update_cmd(flag,flag+" "+this.value);
        } else {
          update_cmd(flag,"");
        }
        update_fs(e);
      };
    </script>
  </xsl:template>

  <xsl:template match="choice">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
        <select id="choice{@flag}">
          <xsl:for-each select="alt">
            <option><xsl:value-of select="@name" /></option>
          </xsl:for-each>
        </select>
      </td>
      <xsl:call-template name="descr" />
    </tr>
    <script type="text/javascript">
      document.getElementById("choice<xsl:value-of select="@flag"/>").onchange =
      function (e) {
        var flag = '<xsl:value-of select="@flag"/>';
        update_cmd(flag,flag+" "+this.value);
      };
    </script>
  </xsl:template>

  <xsl:template match="option">
    <tr>
      <xsl:call-template name="flag" />
      <td class="input">
        <input type="checkbox" id="option{@flag}" value="{@flag}" />
      </td>
      <xsl:call-template name="descr" />
    </tr>
    <script type="text/javascript">
      document.getElementById("option<xsl:value-of select="@flag"/>").onchange =
      function (e) {
        var flag = '<xsl:value-of select="@flag"/>';
        if (this.checked) update_cmd(flag,flag); else update_cmd(flag,"");
      };
    </script>
  </xsl:template>

  <xsl:template match="cli">
    <html>
      <head>
        <title>gloc links our creativity! carpe ignem!</title>
        <script src="CodeMirror2/lib/codemirror.js"></script>
        <script src="CodeMirror2/mode/clike/clike.js"></script>
        <script src="CodeMirror2/keymap/emacs.js"></script>
        <script src="gloc_platform_js.js"></script>
        <script src="_build/gloc_js.d.js"></script>
        <link rel="stylesheet" type="text/css" href="CodeMirror2/lib/codemirror.css" />
        <!-- 
<style type="text/css">
          body { font-family: sans-serif }
          .flag, .input { text-align: right }
          #gloc { top: 20px; right: 10px; position: fixed }
          .fs { width: 50% }
          #gloc-cmd { font-size: 24pt }
          h2 { display: inline }
          .closefile { background-color: red }
          .hidesource { background-color: yellow }
          .showsource { background-color: green; color: white }
          .CodeMirror { border-right: 1px solid #ccc }
          .readonly .CodeMirror { border-color: #c66 }
          .CodeMirror-scroll { height: auto; overflow: visible; }
          .suffix { color: #999 }
        </style>
 -->
        <link rel="stylesheet" href="gloc.css" type="text/css" />
      </head>
      <body onload="init()">
      <div id="header"><h1>gloc<!--<span class="suffix">ode</span>-->&#xA0;<xsl:value-of select="@version"/>
          by <a href="http://ashimagroup.net/"><xsl:value-of select="@distributor"/></a>
          </h1></div>
        <div id="gloc">
          
          <form name="gloc" id="gloc-cli">
            <table>
              <xsl:apply-templates />
              <tr>
                <td colspan="3" style="text-align: center">
                  <input type="submit" value="gloc" id="gloc-cmd" />
                </td>
              </tr>
              <tr>
                <td colspan="3" style="text-align: center" id="gloc-cmd-copy">
                  gloc
                </td>
              </tr>
            </table>
          </form>
        </div>
        <div id="fs">
        </div>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
