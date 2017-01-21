#!/bin/sh

pandoc -i -t beamer talk.rst -o slides.pdf --highlight-style=pygments
