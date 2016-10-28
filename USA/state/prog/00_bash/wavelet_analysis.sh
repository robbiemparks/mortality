#!/bin/bash

# this script
# runs a nationalised wavelet analysis
# runs a state wavelet analysis

clear

declare -i start=1982
declare -i end=2013
declare -i numsim=10
declare country="USA"

#################################################
# 1. NATIONALISED WAVELET ANALYSIS
#################################################

echo "starting nationalised wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_national.R $start $end $numsim

#################################################
# 1. STATE WAVELET ANALYSIS
#################################################

echo "starting state wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_state.R $start $end $numsim

