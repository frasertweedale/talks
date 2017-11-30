SVG_CMD() {
  [ -z "$2" ] && DIR=. || DIR=$2
  inkscape -D -z --file="${DIR}/$1.svg" --export-pdf="$1-ARTIFACT.pdf" --export-latex
}
