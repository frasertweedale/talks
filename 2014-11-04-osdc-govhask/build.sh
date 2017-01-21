#!/bin/sh

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--variable title:"Rapid app development with Haskell" \
	--variable subtitle:"A GovHack experience report" \
	--variable author:"Fraser Tweedale (@hackuador)"
