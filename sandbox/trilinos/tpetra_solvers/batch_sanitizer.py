#!/usr/bin/env python
"""
    Script for removing extra info from tpetra_solvers output
"""


import os, sys
dir = sys.argv[1]

for file in os.listdir(dir):
    if file.endswith(".out"):
        with open(file, 'r') as f:
            with open(file[:-4] + '.cln', 'w') as out:
                for line in f:
                    if ".mtx" in line:
                        out.write(line)
                    elif "Time" in line:
                        out.write(line)


