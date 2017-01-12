#!/bin/bash

# this script
# formats population files and creates rates for Japan

clear

declare -i start=1981
declare -i end=2009

#################################################
# 1. PROCESS DATA
#################################################

echo "preparing monthly death rates for years $start2 - $end2";

Rscript ~/git/mortality/Japan/pref/prog/prep_data/prep_data.R $start $end

