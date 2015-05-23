#!/usr/bin/env cram-python
# This script requires all matrices to be in PETSc binary format in the petsc subdir

import sys, os, glob
from solvers import *

# Directory contaning the *.petsc matrices:
wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
# Directory for storing results:
tdir=wdir+'timing/'
cdir=os.getcwd() 

#nprocs = 4096    # run with qsub -n 256 --proccount 4096  --mode c16 -t 60
nprocs = 2048    # run with qsub -n 128 --proccount 2048 --mode c16 -t 60
#nprocs = 1024    # run with qsub -n 256 --proccount 1024 --mode c4 -t 60
p = 16
matrices = glob.glob(wdir+'petsc/*.petsc')
donelist=[]
if os.path.exists(wdir+'DONE'):
  donelist=open(wdir+'DONE','r').readlines()

solveropts = getsolvers()
buf ="#!/bin/sh\n\n"

totalprocs = 1
env = os.environ
for hash, solver_optstr in solveropts.items():
  for matname in donelist:
    matname = matname.strip()
    if totalprocs > nprocs: break
    lockfile = tdir + '.%s.%s' % (matname, str(hash))
    logfile = tdir + '%s.%s.log' % (matname, str(hash))
    if os.path.exists(lockfile) or os.path.exists(logfile): continue
    opts = ['-f ',wdir+'petsc/'+matname+'.petsc', '-hash', hash, solver_optstr]
    cmd = os.path.join(cdir,'parallel-bgq')
    buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + logfile  + ' 2>&1\n'
    #print cmd + ' ' + ' '.join(opts)
    totalprocs += p

f = open('parallel%d.sh' %p,'w')
f.write(buf)
f.close()
 
