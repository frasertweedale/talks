#!/bin/sh

SVG_CMD() {
	rsvg-convert --width=1600 "$1.svg" --output "$1-ARTIFACT.png"
}

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--template=my.beamer \
	--variable title:"Haskell FFI Basics" \
	--variable author:"Fraser Tweedale\\\\
		@hackuador"
