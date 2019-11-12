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

# MAIN FIGURES

#################################################
# Figures 1 and 2 (mortality data summary)
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figures 1 and 2 for injury paper $start - $end";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons_subcod.R $start $end

# LEGACY (no need to run but left just in case want to summarise by intentional and unintentional)
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons.R $start $end

#################################################
# Figures 3 (additional deaths) and 4A/B (relative risk)
# ALSO Extended Data Figures 3 and 4
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figures 3 and 4 and Extended Data Figures 3 and 4 for injury paper $start - $end";

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_plot_data_cod_1var.R $start $end $country $model $dname $metric $contig $draws

# EXTENDED DATA FIGURES

#################################################
# Extended Data Figure 1 (anomaly schematic)
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting Extended Data Figure 1 for injury paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R $start $end $dname $metric

#################################################
# Extended Data Figure 2 (anomaly summary)
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting Extended Data Figure 2 for injury paper $start - $end";

Rscript ~/git/climate/countries/USA/prog/15_anomaly_summaries/anomaly_summaries.R $start $end $dname $metric

#################################################
# Extended Data Figures 3 and 4
#################################################

# SEE ABOVE

#################################################
# Extended Data Figure 5
#################################################

echo "plotting Extended Data Figure 5 for injury paper $start - $end";


Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_transport_ons_subcod.R $start $end


# EXTENDED DATA TABLES

#################################################
# Supplementary table XX
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting supplementary table XX injury paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/14_comparing_max_min/comparing_max_min.R $start2 $end 'mean' $dname





#################################################
# Tables XX-XX (not currently in paper)
#################################################

echo "populating table XX injury paper $start - $end";

# to correct directory
#cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_human_readable_cod_1var.R $start $end $country $model $dname $metric $contig $draws