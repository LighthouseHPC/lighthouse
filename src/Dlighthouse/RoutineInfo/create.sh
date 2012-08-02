#!/bin/bash/
#Program:
#       Generate the routine info pages (.txt) in the RoutineTxt/ folder
#Created on 07/25/2012 by Sa-Lin Cheng Bernstein


PATH=/Applications/djangostack-1.3.1-4/mysql/bin:/Applications/djangostack-1.3.1-4/python/bin:/Applications/djangostack-1.3.1-4/mysql/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/texbin:/usr/X11/bin

export PATH

echo "Running 'routinePages.py' to generate the .txt files..."
python routinePages.py

echo ".txt files are created!"

echo "Running 'routineList.py' to create routine_info.csv for the database..."

python routineList.py

echo "routine_info.csv is created!"
echo "now you can run load_data.py."
