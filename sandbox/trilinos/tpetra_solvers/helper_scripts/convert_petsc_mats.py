#!/usr/bin/env python
# Converts MOOSE matrices to mtx, depends on petsc2mm.py

import time
import os
import subprocess
import sys

start_time = time.clock()

total = 0
count = 0
root_dir = sys.argv[1] 
sys.stdout.flush()
for root, dirs, files in os.walk(root_dir):
    for fname in files:
        if fname.endswith('.mat'):
            total += 1
            if total % 100 is 0:
                sys.stdout.flush()
                print(total)

sys.stdout.flush()
start = time.time()
for root, dirs, files in os.walk(root_dir):
    for fname in files:
        if fname.endswith('.mat'):
            count += 1
            if os.path.isfile(os.path.join(root,fname) + '.mtx') is False:
                subprocess.call(["./petsc2mm.py", os.path.join(root,fname)])
            if count % 50 is 0:
                print time.time() - start_time, 'seconds'
                start_time = time.time() 
                sys.stdout.flush()
                print('%s %s\n' % (count,total))
                print(': ' + os.path.join(root, fname) + '\n')
