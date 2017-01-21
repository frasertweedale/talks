#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

PANDOC_BIN=/home/ftweedal/dev/pandoc/.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/pandoc/pandoc
$PANDOC_BIN talk.rst --to=beamer -o slides.pdf -s \
  --highlight-style pygments \
  -V classoption:"aspectratio=169" \
  -V classoption:"notes=show" \
  -V title:"[Pan]Dr. Slidelove" \
  -V subtitle:"or: How I Learned to Stop Worrying and Love the Beamer" \
  -V author:"Fraser Tweedale\\\\
    @hackuador" \
  -V institute:"Red Hat, Inc." \
  -V date:"\\today" \
  -V header-includes:"\\hypersetup{colorlinks,linkcolor=,urlcolor=purple}" \
  -V header-includes:"\\usefonttheme[onlymath]{serif}" \
  --template my.beamer \
