#!/bin/sh
. ../share/subr.sh
SVG_CMD cc-by ../share
SVG_CMD Logo-RedHat-A-Color-RGB ../share
SVG_CMD kerb-ap-req-post ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-ap-req ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-as-exchange-post ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-as-rep ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-as-req ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-parties ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-tgs-rep-post ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-tgs-rep ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-tgs-req ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-pkinit-as-rep ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-pkinit-as-req ../2023-02-04-fosdem-pkinit/img
SVG_CMD kerb-pkinit-as-exchange-post ../2023-02-04-fosdem-pkinit/img
pdflatex slides
