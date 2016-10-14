#!/bin/bash

# this script
# plots population against time for counties by state
# plots death rates against time by state

clear

declare -i start=1982
declare -i end_=2013


#################################################
# 1. POPULATION OVER TIME BY STATE AND COUNTY
#################################################

echo "plotting population against time by county";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/plot_county_against_time.R

#################################################
# 2. DEATH RATES OVER TIME BY STATE
#################################################

declare metric1='mean'
declare metric2='sd'

echo "plotting raw death rates by state for years $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore.R $start $end
