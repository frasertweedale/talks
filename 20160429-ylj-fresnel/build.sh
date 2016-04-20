#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

SVG_CMD Logo_RH_RGB_Default
SVG_CMD cc-by

PANDOC_BIN=/home/ftweedal/dev/pandoc/.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/pandoc/pandoc
$PANDOC_BIN talk.rst --to=beamer -o slides.pdf -s \
  --highlight-style pygments \
  -V classoption:"aspectratio=169" \
  -V title:"Parsing and pretty printing with prisms" \
  -V author:"Fraser Tweedale\\\\
    \\texttt{@hackuador}\\\\
    \\bigskip
    \\def\\svgwidth{2cm}
    \\input{Logo_RH_RGB_Default-ARTIFACT.pdf_tex}" \
  -V institute:"Red Hat, Inc." \
  -V date:"April 29, 2016" \
  -V header-includes:"\\usepackage[normalem]{ulem}" \
  -V header-includes:"\\hypersetup{colorlinks,linkcolor=,urlcolor=purple}" \
  -V header-includes:"\\usefonttheme[onlymath]{serif}" \
  -V header-includes:"\\DeclareUnicodeCharacter{00A0}{~}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2227}{$\\wedge$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2228}{$\\vee$}" \
  #-V classoption:"notes=show" \
