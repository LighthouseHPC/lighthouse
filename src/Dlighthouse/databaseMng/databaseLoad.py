##### Load data into the database #####

import os

### load data to Driver_routineinfo table 
os.system('python RoutineInfo/load_data.py')


### load data to
### Driver_linearequation_simple, Driver_linearequation_expert, Driver_linearequation_driver
os.system('python Driver/load_data.py')


### load data to
### Computational_linearequation_computational, Computational_linearequation_factor
### Computational_linearequation_solve, Computational_linearequation_condition_number
### Computational_linearequation_error_bound, Computational_linearequation_invert
### Computational_linearequation_equilibrate
os.system('python Computational/load_data.py')


### load data to
### Combine_linearequation_only
os.system('python Combine/load_data.py')