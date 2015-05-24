#!/usr/bin/env cram-python
# This script requires all matrices to be in PETSc binary format in the petsc subdir
from cram import *
import sys, os, glob, random
from solvers import *

cf = CramFile('parallel-cram.job','w')

# Directory contaning the *.petsc matrices:
wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
# Directory for storing results:
tdir=wdir+'timing/'
cdir=os.getcwd() 

nprocs = 256    # run with qsub -n 256 --proccount 4096 --mode c16  -t 1:00:00 --env CRAM_OUTPUT=ALL:CRAM_FILE=/gpfs/mira-fs0/projects/PEACEndStation/norris/lighthouse/sandbox/petsc/new/parallel-cram.job ../parallel-cram
num_matrices = 55

p = 16
matrices = glob.glob(wdir+'petsc/*.petsc')
donelist=[]
if os.path.exists(wdir+'DONE'):
  donelist=open(wdir+'DONE','r').readlines()

donelist = list(reversed(donelist))

solveropts = getsolvers()
#buf ="#!/bin/sh\n\n"

totalprocs = 1
env = os.environ
hashlist = solveropts.keys()
random.shuffle(hashlist)
for hash in hashlist:
  solver_optstr = solveropts[hash]
  nm = 0
  for matname in donelist:
    if nm >= num_matrices: break
    matname = matname.strip()
    if totalprocs > nprocs: break
    lockfile = tdir + '.%s.%s' % (matname, str(hash))
    logfile = tdir + '%s.%s.log' % (matname, str(hash))
    if os.path.exists(lockfile) or os.path.exists(logfile): continue
    opts = ['-f ',wdir+'petsc/'+matname+'.petsc', '-hash', hash] +  solver_optstr.strip().split(' ')
    cmd = os.path.join(cdir,'parallel-cram')
    nm += 1
    #buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + logfile  + ' 2>&1\n'
    #print cmd + ' ' + ' '.join(opts)

    # CRAM script
    env["DARSHAN_DISABLE"] = "1"
    cf.pack(p,cmd, opts, env)
  totalprocs += 1

cf.close()


 
