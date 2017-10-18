#!/bin/sh

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--variable classoption:"aspectratio=169" \
	--variable title:"The Best Test Data is Random Test Data" \
	--variable subtitle:"An introduction to property-based testing" \
	--variable author:"Fraser Tweedale\\\\
		Red Hat, Inc.\\\\
		@hackuador"
