#!/bin/bash

# this script
# runs a nationalised wavelet analysis

clear

declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. NATIONALISED WAVELET ANALYSIS
#################################################

echo "starting nationalised wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet.R $start $end

