#!/bin/bash

# this script
# runs a nationalised wavelet analysis
# runs a state wavelet analysis
# processes results
# plots results on maps etc.

clear

declare -i start=1982
declare -i end=2013
declare -i start2=1982
declare -i end2=2013
declare dname="t2m"
declare metric="mean"


#################################################
# 1. SEASONALITY INDEX
#################################################

clear

echo "starting seasonality index analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index.R $start $end $start2 $end2 $dname $metric
