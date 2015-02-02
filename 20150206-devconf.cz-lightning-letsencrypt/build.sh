#!/bin/sh

pandoc -t beamer talk.rst -o slides.pdf -s --highlight-style=pygments \
	--variable title:"Let's Encrypt!" \
	--variable author:"Fraser Tweedale\\\\
		Red Hat, Inc.\\\\
		@hackuador"
