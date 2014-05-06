##### Load data into the database #####

import os
currentDir = os.getcwd()

os.chdir("guided")
os.system('python load_data.py')
os.chdir("../")

