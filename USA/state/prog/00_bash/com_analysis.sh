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

# runs COM analysis
Rscript ~/git/mortality/USA/state/prog/com/com_analysis.R $start $end

# runs anti-COM analysis
Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis.R $start $end

