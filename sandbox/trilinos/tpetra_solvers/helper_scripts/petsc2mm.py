#!/usr/bin/env python

import os, sys
import PetscBinaryIO
from scipy.io import mmwrite

if len(sys.argv) is 2:
    in_file = sys.argv[1]
else:
    print('File name must be specified as argument\n')
    exit()

io = PetscBinaryIO.PetscBinaryIO()
fh = open(in_file)
objecttype = io.readObjectType(fh)
if objecttype == 'Mat':
    A = io.readMatSciPy(fh)
    mmwrite(in_file + '.mtx', A)
