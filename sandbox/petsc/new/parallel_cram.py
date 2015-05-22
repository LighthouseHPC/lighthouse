#!/usr/bin/env cram-python
# This script requires all matrices to be in PETSc binary format in the petsc subdir

import sys, os, glob
from cram import *

cf = CramFile('parallel-cram.job','w')
wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
tdir=wdir+'timing/'
cdir=os.getcwd() 

nprocs = 2048    # run with qsub -n 512 --proccount 2048 -O Anamod --mode c4 -t 4:00:00
#nprocs = 1024    # run with qsub -n 256 --proccount 1024 -O Anamod --mode c4 -t 1:00:00
p = 16
matrices = glob.glob(wdir+'petsc/*.petsc')
donelist=[]
if os.path.exists(wdir+'DONE'):
  donelist=open(wdir+'DONE','r').readlines()

buf ="#!/bin/sh\n\n"

totalprocs = 1
env = os.environ
for matname in donelist:
  if totalprocs > nprocs: break
  #matname=os.path.basename(m).replace('.petsc.info','')
  #if not matname in donelist: continue
  # Pack cram invocations.
  # Usage:
  #   cf.pack(<num procs>, <working dir>, <command-line arguments>, <environment>)
  #env["MY_SPECIAL_VAR"] = "my_value"
  opts = ['-f ',wdir+'petsc/'+matname.strip()+'.petsc']
  cmd = os.path.join(cdir,'parallel-bgq')
  buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + tdir + matname.strip() + '.log\n'
  print cmd + ' ' + ' '.join(opts)
  cf.pack(p, cmd, opts, env)
  totalprocs += p

f = open('parallel%d.sh' %p,'w')
f.write(buf)
f.close()
cf.close()
 
