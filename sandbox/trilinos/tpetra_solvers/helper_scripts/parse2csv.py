#!/usr/bin/env python

import re,sys,os

root_dir = sys.argv[1] 

with open('results.csv', 'w') as results_file:
    results_file.write('Root Location: ' + root_dir + '\n')
    for fname in os.listdir(root_dir):
        print fname
        with open(root_dir + fname, 'r') as reading:
            for i, line in enumerate(reading):
                if line.find('converge') >= 0:
                    results_file.write(line)
                elif line.find('Error') >= 0:
                    results_file.write(line)
