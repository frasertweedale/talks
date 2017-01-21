#!/bin/sh

SVG_CMD() {
	rsvg-convert --width=1600 "$1.svg" --output "$1-ARTIFACT.png"
}

#SVG_CMD letsencrypt-logo-horizontal

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--template=my.beamer \
	--variable title:"Parser combinators from scratch" \
	--variable author:"Fraser Tweedale\\\\
		@hackuador"
