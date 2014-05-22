##### Load data into the database #####

import os
currentDir = os.getcwd()

### load data to
### lapack_le_linearequation_simple, lapack_le_linearequation_expert, lapack_le_linearequation_driver
os.chdir("Eprobs")
os.system('python load_data.py')
os.chdir("../")

