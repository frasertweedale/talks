#!/bin/sh
. ../share/subr.sh
SVG_CMD cc-by ../share
SVG_CMD Logo-RedHat-A-Color-RGB ../share
SVG_CMD Container_Evolution img
SVG_CMD figure-cri img
SVG_CMD figure-cri-crio-oci img
SVG_CMD figure-cri-crio-runc img
SVG_CMD figure-user-namespaces img
pdflatex slides
