#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

pandoc -t beamer wrap.rst -o slides.pdf -s \
  --highlight-style=pygments \
  -V classoption:"aspectratio=169" \
  -V title:"Wrap-up" \
  -V author:"Fraser Tweedale\\\\
    \\texttt{@hackuador}" \
  -V date:"January 17, 2017" \
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
  -V header-includes:"\\DeclareUnicodeCharacter{2605}{$\\star$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{1D53D}{\\mathbb{F}}" \
