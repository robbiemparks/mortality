#!/bin/bash

# this script
# runs a nationalised COM analysis using circular statistics

clear

declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. NATIONALISED WAVELET ANALYSIS
#################################################

echo "starting nationalised COM analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/circular/circular.R $start $end

