#!/bin/bash

# this script 
# parses the raw tape data
# removes foreign deaths and recodes age
# formats data in same way as old code in Stata for next stage of processing
# check old data against new data, if it exists

clear

declare -a years=($(seq 2014 2016))
# 2014-2016 data processing values
declare us='USAllCnty.txt'
declare ps='PSAllCnty.txt'
# 2013 data processing values
#declare us='USPART2'
#declare ps='PSPART2'

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

for year in "${years[@]}"; do

#################################################
# 1. EXTRACT FROM TAPES
#################################################
#
#echo "processing raw files from $year";
#
#echo "extacting files from tape format for $year";
#
## extracts file for year from tape format into something intelligible
#Rscript ~/git/mortality/USA/state/prog/format_mort/format_mort.R $year $us
#Rscript ~/git/mortality/USA/state/prog/format_mort/format_mort.R $year $ps

#################################################
# 2. REMOVE FOREIGN DEATHS
#################################################
#
#echo "removing foreign deaths for $year";
#
## filter deaths of people who were not residents of the USA
#Rscript ~/git/mortality/USA/state/prog/format_mort/rmv_foreign_dths.R $year $us
#Rscript ~/git/mortality/USA/state/prog/format_mort/rmv_foreign_dths.R $year $ps

##################################################
## 3. RECODE AGE
##################################################

#echo "recoding age for $year";
#
## recode age format
#Rscript ~/git/mortality/USA/state/prog/format_mort/recode_age.R $year $us
#Rscript ~/git/mortality/USA/state/prog/format_mort/recode_age.R $year $ps
#
##################################################
## 4. FORMAT DATA FOR NEXT STAGE OF PROCESSING
##################################################

echo "emulating inherited data format for $year";

# emulate Harvard output data form
Rscript ~/git/mortality/USA/state/prog/format_mort_cod/reformat_data_cod.R $year

#################################################
# 5. CHECK OLD AGAINST NEW DATA EXPORT
#################################################

#echo "compare old against new data for $year";
#
## check if the old data and new data match up
##Rscript ~/git/mortality/USA/state/prog/format_mort/old_against_new.R $year
#

done;
