#!/bin/sh

SVG_CMD() {
	rsvg-convert --width=1600 "$1.svg" --output "$1-ARTIFACT.png"
}

#SVG_CMD letsencrypt-logo-horizontal

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--template=my.beamer \
	--variable title:"Parametricity and other principles" \
	--variable author:"Fraser Tweedale\\\\
		@hackuador"
