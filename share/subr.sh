SVG_CMD() {
  [ -z "$2" ] && DIR=. || DIR=$2
  IN="${DIR}/$1.svg"
  OUT="$1-ARTIFACT.pdf"
  [ ! -e $OUT -o $IN -nt $OUT ] \
    && inkscape -D -z --file=$IN --export-pdf=$OUT --export-latex
}
