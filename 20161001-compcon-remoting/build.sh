#!/bin/sh

SVG_CMD() {
	inkscape -D -z --file="$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}

SVG_CMD Logo_RH_RGB_Default
SVG_CMD cc-by
SVG_CMD Simple_world_map_au
SVG_CMD Simple_world_map_au_cz_us
SVG_CMD Simple_world_map_all

PANDOC_BIN=/home/ftweedal/dev/pandoc/.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/pandoc/pandoc
$PANDOC_BIN -t beamer talk.rst -o slides.pdf -s \
  --highlight-style=pygments \
  -V classoption:"aspectratio=43" \
  -V title:"The Joys and Trials of Remote Work" \
  -V author:"Fraser Tweedale\\\\
    %\\texttt{ftweedal@redhat.com}\\\\
    \\bigskip
    \\def\\svgwidth{2cm}
    \\input{Logo_RH_RGB_Default-ARTIFACT.pdf_tex}" \
  -V date:"October 1, 2016" \
  -V header-includes:"\\usefonttheme[onlymath]{serif}" \
  -V header-includes:"\\setbeamertemplate{navigation symbols}{}" \
  -V header-includes:"\\hypersetup{colorlinks,linkcolor=,urlcolor=purple}" \
  -V header-includes:"\\DeclareUnicodeCharacter{00A0}{~}" \
  -V header-includes:"\\DeclareUnicodeCharacter{03B4}{$\\delta$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{03B5}{$\\varepsilon$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{03C9}{$\\omega$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2124}{\\mathbb{Z}}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2208}{$\\in$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2209}{$\\notin$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{220B}{$\\ni$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2227}{$\\wedge$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2228}{$\\vee$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2234}{$\\therefore$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2264}{$\\leq$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{2265}{$\\geq$}" \
  -V header-includes:"\\DeclareUnicodeCharacter{1D53D}{\\mathbb{F}}" \
