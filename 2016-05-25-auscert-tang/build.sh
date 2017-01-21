#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

SVG_CMD Logo_RH_RGB_Default
SVG_CMD USB_Icon
SVG_CMD identity-secure
SVG_CMD Lagrange_polynomial
SVG_CMD cc-by

pandoc -t beamer talk.rst -o slides.pdf -s \
  --highlight-style=pygments \
  -V classoption:"aspectratio=169" \
  -V title:"Tang and Clevis" \
  -V subtitle:"Shackling Secrets to the Network" \
  -V author:"Fraser Tweedale\\\\
    \\texttt{@hackuador}\\\\
    \\bigskip
    \\def\\svgwidth{2cm}
    \\input{Logo_RH_RGB_Default-ARTIFACT.pdf_tex}" \
  -V date:"May 25, 2016" \
  -V header-includes:"\\usefonttheme[onlymath]{serif}" \
  -V header-includes:"\\setbeamertemplate{navigation symbols}{}" \
  -V header-includes:"\\hypersetup{colorlinks,linkcolor=,urlcolor=purple}" \
  -V header-includes:"\\DeclareUnicodeCharacter{00A0}{~}" \
  -V header-includes:"\\DeclareUnicodeCharacter{03B4}{$\\delta$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{03B5}{$\\varepsilon$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{03C9}{$\\omega$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2124}{\\mathbb{Z}}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2227}{$\\wedge$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2228}{$\\vee$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2234}{$\\therefore$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{1D53D}{\\mathbb{F}}" \
