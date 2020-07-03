<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="html">

<xsl:key name="main" match="html:main" use="''"/>

<xsl:template match="/html:*">
  <xsl:copy-of select="key('main', '')[1]"/>
</xsl:template>

</xsl:stylesheet>
