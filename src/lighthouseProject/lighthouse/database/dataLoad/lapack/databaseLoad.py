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
os.chdir(database_path+"/lapack_le/Driver")
os.system('python load_data.py')
os.chdir(database_path)


### load data to table
### lapack_le_linearequation_computational, lapack_le_linearequation_factor
### lapack_le_linearequation_solve, lapack_le_linearequation_condition_number
### lapack_le_linearequation_error_bound, lapack_le_linearequation_invert
### lapack_le_linearequation_equilibrate
os.chdir(database_path+"/lapack_le/Computational")
os.system('python load_data.py')
os.chdir(database_path)


### load data to table
### lapack_le_only
os.chdir(database_path+"/lapack_le/Combine")
os.system('python load_data.py')
os.chdir(database_path)


### load data to table
### lapack_le_arg
os.chdir(database_path+"/lapack_le/Arguments")
os.system('python load_data.py')
os.chdir(database_path)





##### ------------ for LAPACK_eigen tables ---------------- #####
### load data to table
### lapack_eigen
os.chdir(database_path+"/lapack_eigen/guided")
os.system('python load_data.py')
os.chdir(database_path)



##### ------------ for LAPACK_svd tables ---------------- #####
### load data to table
### lapack_svd
os.chdir(database_path+"/lapack_svd/guided")
os.system('python load_data.py')
os.chdir(database_path)