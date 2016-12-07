#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(75)
declare -a sexes=(2)
declare -a sexstrings=('male' 'female')
declare -i start=1982
declare -i end=2013
declare -a pwls=(1 2)
declare -i type=2
declare -i forecast_length=5
declare -i knot=2
declare country="USA"

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

for pwl in "${pwls[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, pwl $pwl, forecast length $forecast_length, years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast.R $age $sex $start $end $pwl $type $forecast_length $knot

done; done; done;

#################################################
# 2. COMBINE RESULTS
#################################################

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior.R $start $end $country 2

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

echo "plotting parameters from INLA model $model years $start - $end";

# PLOT PARAMETERS CODE
