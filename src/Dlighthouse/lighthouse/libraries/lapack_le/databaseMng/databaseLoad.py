##### Load data into the database #####

import os
currentDir = os.getcwd()

### load data to lapack_le_routineinfo table
os.chdir("RoutineInfo")
os.system('python load_data.py')
os.chdir("../")


### load data to
### lapack_le_linearequation_simple, lapack_le_linearequation_expert, lapack_le_linearequation_driver
os.chdir("Driver")
os.system('python load_data.py')
os.chdir("../")


### load data to
### lapack_le_linearequation_computational, lapack_le_linearequation_factor
### lapack_le_linearequation_solve, lapack_le_linearequation_condition_number
### lapack_le_linearequation_error_bound, lapack_le_linearequation_invert
### lapack_le_linearequation_equilibrate
os.chdir("Computational")
os.system('python load_data.py')
os.chdir("../")


### load data to
### lapack_le_only
os.chdir("Combine")
os.system('python load_data.py')
os.chdir("../")


### load data to
### lapack_le_arg
os.chdir("Arguments")
os.system('python load_data.py')
os.chdir("../")