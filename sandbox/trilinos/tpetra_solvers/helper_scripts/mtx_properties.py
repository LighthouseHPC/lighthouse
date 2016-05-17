#!/usr/bin/env python
"""
    Creates two files: one for dimensions, one for matrix-market types
        (Real, General, Symmetric, etc.) and fills them in by starting 
        a walk from a specified directory. Useful for determining the 
        overall properties of a large collection of data.

    USAGE: ./mtx_properties.py <starting_directory> <output_directory> 
"""
import time
import os
import subprocess
import sys

start_time = time.clock()
root_dir = sys.argv[1] 
output_dir = sys.argv[2]
sys.stdout.flush()
count = 0
total = 0

with open(output_dir + '/mtx-types.txt', 'w') as line_file:
    line_file.write('Root Location: ' + root_dir)
    with open(output_dir + '/mtx-dims.txt', 'w') as dim_file:
        dim_file.write('Root Location: ' + root_dir)
        for root, dirs, files in os.walk(root_dir):
            for fname in files:
                if fname.endswith('.mtx'):
                    total += 1
                    if total % 100 is 0:
                        sys.stdout.flush()
                        print(total)
        for root, dirs, files in os.walk(root_dir):
            for fname in files:
                if fname.endswith('.mtx'):
                    count += 1
                    with open(os.path.join(root,fname), 'r') as f:
                        first_line = f.readline()
                        words = first_line.split()
                        words = words[2:]
                        line_file.write(fname + ' ' + ' '.join([str(i) for i in words]) + '\n')
                        if count % 50 is 0:
                            print time.time() - start_time, 'seconds'
                            start_time = time.time() 
                            sys.stdout.flush()
                            print('%s %s\n' % (count,total))
                        for line in f:
                            if line.startswith('%') is False:
                                dim_file.write(fname + ' ' + line) 
                                break
