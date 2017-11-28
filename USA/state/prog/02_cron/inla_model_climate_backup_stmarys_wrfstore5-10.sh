#!/bin/bash

# backs up all the climate variable stuff from INLA models from stmarys to wrfproc1-10
@daily rsync -avz -e ssh /home/rmp15/data/mortality/US/state/climate_effects/* rmp15@wrfproc1-10.sp.ph.ic.ac.uk:~/data/mortality/US/state/climate_effects/

# backs up all the mortality data from stmarys to wrfproc1-10
@weekly rsync -avz -e ssh /home/rmp15/data/mortality/* rmp15@wrfproc1-10.sp.ph.ic.ac.uk:~/data/mortality/

# backs up all the climate data from stmarys to wrfproc1-10
@weekly rsync -avz -e ssh /home/rmp15/data/climate/* rmp15@wrfproc1-10.sp.ph.ic.ac.uk:~/data/climate/
