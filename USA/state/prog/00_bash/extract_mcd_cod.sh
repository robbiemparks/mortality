#!/bin/bash

# this script
# processes MCD files to ensure that they include cause of death, county, and month

clear

declare -a years=(seq($(1982 2012)))
declare -a years2=(2013)
declare -i start=1982
declare -i end= 2013

#################################################
# 1. PROCESS MCD FILES
#################################################

for year in "${years[@]}"; do

echo "formatting mcd files for $year";

Rscript ~/git/mortality/USA/state/prog/format_mort_cod/prep_mcd_counties_cod.R $year

done;

for year2 in "${years2[@]}"; do

echo "formatting mcd files for $year2";

Rscript ~/git/mortality/USA/state/prog/format_mort_cod/reformat_data_cod.R $year

done;

#################################################
# 2. PROCESS DEATH RATES (NOT FINISHED!!)
#################################################

echo "preparing monthly death rates for years $start2 - $end2";

Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_cod.R $start $end
Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_deaths_adj_cod.R $start $end

#################################################
# 3. EXPLORE DATA (NOT FINISHED!!)
#################################################

echo "exploring death rates for years $start2 - $end2";

Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_cod.R $start $end
