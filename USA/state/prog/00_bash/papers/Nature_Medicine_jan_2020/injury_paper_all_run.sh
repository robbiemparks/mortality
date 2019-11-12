#!/bin/bash

# this script
# runs necessary scripts for injury paper

clear

declare -a sexstrings=('male' 'female')
declare -a model=(10)
declare -i start=1980
declare -i end=2017
declare -i start2=1979
declare country="USA"
declare dname="t2m"
declare metric="meanc4"
declare -i fast=1
declare -i contig=1
declare -a draws=(5000)

#################################################
# Mortality data prep
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "preparing monthly death rates in injury sub-sub-causes of deaths for years $start - $end";

# thesis (old and changed since Nature Medicine revisions)
#Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_subsubcauses_injuries_ons.R $start $end

# Main analysis revisions to Nature Medicine paper (September 2019)
Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_subsubcauses_injuries_ons_revisions.R $start $end

# Side analysis for transport injuries split into road and non-road for Nature Medicine paper (September 2019)
Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_subsubcauses_injuries_ons_revisions_transport_only.R $start $end

#################################################
# Temperature data prep
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

# SEE ~/git/climate/countries/USA/prog/00_bash/weighted_mean_era5.sh

#################################################
# Model runs
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

# 1. MAIN PAPER MODEL RUNS (model 27)
# SEE ~/git/mortality/USA/state/prog/00_bash/inla/medbio_injury_run.run

# 2. SENSITIVITY MODEL RUNS WITH ALTERNATIVE HYPERPRIOR (model 28)
# SEE ~/git/mortality/USA/state/prog/00_bash/inla/medbio_injury_run.run

# 3. SENSITIVITY MODEL RUNS WITH LONG-RUN TEMPERATURE INCLUDED (model 29)
# SEE ~/git/mortality/USA/state/prog/00_bash/inla/medbio_injury_run.run

#################################################
# Additional deaths calculations
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

# 1. DRAWS
# SEE ~/git/mortality/USA/state/prog/00_bash/draws/inla_draws_climate_cod_1var_1-10_era5.sh

# 2. PROCESS DRAWS FOR ADDITIONAL DEATHS
# SEE ~/git/mortality/USA/state/prog/00_bash/additional_deaths/additional_deaths_draws_cod_1var_era5.sh

#################################################
# Plots
#################################################

# SEE ~/git/mortality/USA/state/prog/00_bash/papers/Nature_Medicine_jan_2020/injury_paper_figures.sh
