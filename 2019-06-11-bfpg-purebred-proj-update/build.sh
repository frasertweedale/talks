#!/bin/sh
. ../share/subr.sh
SVG_CMD cc-by ../share
SVG_CMD purebred-logo .
pdflatex slides
