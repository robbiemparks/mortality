#!/usr/bin/python

import pandas as pd
import numpy as np

file_loc = "~/data/mortality/US/state/actual/datus_state_rates_1982_2013.csv"
mb = pd.read_csv(file_loc)
print(mb.shape)
