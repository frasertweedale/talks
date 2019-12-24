#!/bin/sh
. ../share/subr.sh
SVG_CMD cc-by ../share
SVG_CMD Logo-RedHat-A-Color-RGB ../share
SVG_CMD USB_Icon .
SVG_CMD identity-secure .
SVG_CMD Lagrange_polynomial .
pdflatex slides
