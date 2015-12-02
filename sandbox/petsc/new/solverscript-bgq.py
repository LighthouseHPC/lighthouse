#!/usr/bin/env python
# This script requires all matrices to be in PETSc binary format in the petsc subdir

import sys, os, glob, random
from solvers import *


# Directory contaning the *.petsc matrices:
wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
wdir='/home/norris/UFloridaSparseMat/'
# Directory for storing results:
tdir=wdir+'timing/'
cdir=os.getcwd() 

#nprocs = 4096    # run with qsub -n 256 --proccount 4096  --mode c16 -t 60
nprocs = 2048    # run with qsub -n 128 --proccount 2048 --mode c16 -t 60
#nprocs = 1024    # run with qsub -n 256 --proccount 1024 --mode c4 -t 60
p = 16
matrices = glob.glob(wdir+'petsc/*.petsc')
donelist=[]
#if os.path.exists('DONE_TRILINOS'):
#  donelist=open('DONE_TRILINOS','r').readlines()
if os.path.exists('DONE'):
  donelist=open('DONE','r').readlines()
else:
  print "Error: can't find done matrix file"
  exit(1)

import commands
s = commands.getstatusoutput('qstat | grep norris | wc -l')[1]
if (int(s) > 0):
  donelist = list(reversed(donelist))

solveropts = getsolvers()
buf ="#!/bin/sh\n\n"

totalprocs = 1
env = os.environ
hashlist = solveropts.keys()
random.shuffle(hashlist)
for hash in hashlist:
  solver_optstr = solveropts[hash]
  if totalprocs > nprocs: break
  for mat in donelist:
    matname = mat.strip('\n')
    matrixpath=wdir+'petsc/'+matname+'.petsc'
    if not os.path.exists(matrixpath):
      #print "No PETSc matrix:", matrixpath
      continue
    #else:
      #print "PETSc matrix:", matrixpath
    if totalprocs > nprocs: break
    lockfile = tdir + '.%s.%s' % (matname, str(hash))
    logfile = tdir + '/tmp/%s.%s.log' % (matname, str(hash))
    #print "Logfile:", logfile
    if os.path.exists(lockfile) or os.path.exists(logfile): continue
    opts = ['-f ',wdir+'petsc/'+matname+'.petsc', '-hash', hash, solver_optstr]
    cmd = os.path.join(cdir,'parallel-bgq')
    buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + logfile  + ' \n'
    totalprocs += p
    #print cmd + ' ' + ' '.join(opts)


scriptname = 'parallel%d.sh' %p
f = open(scriptname,'w')
f.write(buf)
f.close()
os.system("chmod +x %s" % scriptname)


 
