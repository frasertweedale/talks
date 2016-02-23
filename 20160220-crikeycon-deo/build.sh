#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

SVG_CMD Lagrange_polynomial

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--template=my.beamer \
	--variable title:"Tang and Clevis" \
	--variable subtitle:"Shackling Secrets to the Network" \
	--variable author:"Fraser Tweedale\\\\
		@hackuador" \
	-V date:"February 20, 2016" \
	--variable institute:"Red Hat, Inc" \
