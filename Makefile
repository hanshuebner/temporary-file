doc.html: doc.xml clixdoc.xsl
	xsltproc clixdoc.xsl doc.xml > doc.html