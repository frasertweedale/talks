#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

SVG_CMD identity-secure
SVG_CMD letsencrypt-logo-horizontal
SVG_CMD cc-by

pdflatex slides
