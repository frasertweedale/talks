SVG_CMD() {
  [ -z "$2" ] && DIR=. || DIR=$2
  IN="${DIR}/$1.svg"
  OUT="$1-ARTIFACT.pdf"
  [ ! -e $OUT -o $IN -nt $OUT ] \
    && inkscape --export-area-drawing --export-filename=$OUT --export-latex $IN
}
