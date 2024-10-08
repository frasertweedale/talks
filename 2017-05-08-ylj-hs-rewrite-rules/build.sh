#!/bin/sh

SVG_CMD() {
  [ -z "$2" ] && DIR=. || DIR=$2
  inkscape -D -z --file="${DIR}/$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

SVG_CMD cc-by ../share

PANDOC_BIN=pandoc
$PANDOC_BIN talk.rst --to=beamer -o slides.pdf -s \
  --highlight-style pygments \
  -V classoption:"aspectratio=169" \
  -V title:"Performant polymorphism" \
  -V subtitle:"Rewrite rules in Haskell" \
  -V author:"Fraser Tweedale\\\\
    \\texttt{@hackuador}" \
  -V date:"May 8, 2017" \
  -V header-includes:"\\usepackage[normalem]{ulem}" \
  -V header-includes:"\\hypersetup{colorlinks,linkcolor=,urlcolor=purple}" \
  -V header-includes:"\\usefonttheme[onlymath]{serif}" \
  -V header-includes:"\\DeclareUnicodeCharacter{00A0}{~}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2227}{$\\wedge$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2228}{$\\vee$}" \
  #-V classoption:"notes=show" \
