##### Load data into the database #####

import os
currentDir = os.getcwd()

##### ------------ go to ../lighthouse/database/ ---------------- #####
database_path = os.path.abspath(os.path.join(os.path.dirname(__file__),"../../"))
print database_path


##### ------------ for LAPACK routine info table ---------------- #####
### load data to table
### lapack_routineinfo
os.chdir(database_path+"/RoutineInfo/lapack")
os.system('python load_data.py')
os.chdir(database_path)

##### ------------ for LAPACK_le tables ---------------- #####
### load data to table
### lapack_le_linearequation_simple, lapack_le_linearequation_expert, lapack_le_linearequation_driver
### lapack_le_linearequation_computational, lapack_le_linearequation_factor
### lapack_le_linearequation_solve, lapack_le_linearequation_condition_number
### lapack_le_linearequation_error_bound, lapack_le_linearequation_invert
### lapack_le_linearequation_equilibrate
### lapack_le_only
### lapack_le_arg
os.chdir(database_path+"/lapack_le")
os.system('python databaseLoad.py')
os.chdir(database_path)

##### ------------ for LAPACK_eigen tables ---------------- #####
### load data to table
### lapack_eigen
os.chdir(database_path+"/lapack_eigen")
os.system('python dataManage.py load_data')
os.chdir(database_path)

##### ------------ for LAPACK_svd tables ---------------- #####
### load data to table
### lapack_svd
os.chdir(database_path+"/lapack_svd")
os.system('python load_data.py')
os.chdir(database_path)

##### ------------ for LAPACK_sylvester tables ---------------- #####
### load data to table
### lapack_sylvester
os.chdir(database_path+"/lapack_sylvester")
os.system('python load_data.py')
os.chdir(database_path)

