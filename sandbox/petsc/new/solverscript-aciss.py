#!/usr/bin/env python
# This script requires all matrices to be in PETSc binary format in the petsc subdir

import sys, os, glob, random
from solvers import *
import datetime,time

def resetBuffer():
  #b =  '#!/bin/bash\n\nmodule load mpi/mpich-3.1_gcc-4.9\n\n'
  b =  '#!/bin/bash\n\nmodule load gcc/4.9\n\n'
  b += 'export PETSC_DIR=/home11/bnorris2/petsc/petsc-3.5.3; export PETSC_ARCH=arch-linux2-c-mpich3.1-gcc4.9\n\n'
  b += 'export LD_LIBRARY_PATH=$PETSC_DIR/$PETSC_ARCH/lib:$LD_LIBRARY_PATH\n\n'
  b += 'cd $HOME/UFloridaSparseMat/timing-aciss\n\n'
  return b

def getJobs():
  s = commands.getstatusoutput('qstat | grep norris | grep short | wc -l')[1]
  return int(s)

# Directory contaning the *.petsc matrices:
#wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
wdir='/home11/bnorris2/UFloridaSparseMat/'
# Directory for storing results:
tdir=wdir+'timing-aciss/'
cdir=os.getcwd() 

#nprocs = 4096    # run with qsub -n 256 --proccount 4096  --mode c16 -t 60
nprocs = 2048    # run with qsub -n 128 --proccount 2048 --mode c16 -t 60
#nprocs = 1024    # run with qsub -n 256 --proccount 1024 --mode c4 -t 60
p = 16
matrices = glob.glob(wdir+'petsc/*.petsc')
donelist=[]
# DONE_TRILINOS
if os.path.exists('DONE'):
  donelist=open('DONE','r').readlines()
else:
  print "Error: can't find done matrix file"
  exit(1)

import commands
if (getJobs() > 0):
  donelist = list(reversed(donelist))

solveropts = getsolvers()

buf = resetBuffer()
cleanup = 'wait\n'

totalprocs = 1
env = os.environ
hashlist = solveropts.keys()
random.shuffle(hashlist)
for hashnum in hashlist:
  solver_optstr = solveropts[hashnum]
  if totalprocs > nprocs: break
  for matname in donelist:
    while (getJobs() > 20): time.sleep(60)
    matname = matname.strip()
    matrixpath=wdir+'petsc/'+matname+'.petsc'
    if not os.path.exists(matrixpath):
      #print "No PETSc matrix:", matrixpath
      continue
    #else:
      #print "PETSc matrix:", matrixpath
    if totalprocs > nprocs: break
    lockfile = tdir + '.%s.%s' % (matname, str(hashnum))
    logfile = tdir + '%s.%s.log' % (matname, str(hashnum))
    #print "Logfile:", logfile
    if os.path.exists(lockfile) or os.path.exists(logfile): continue
    opts = [' -f ',wdir+'petsc/'+matname+'.petsc', ' -hash', hashnum, solver_optstr, ' -ksp_view -options_left -log_summary ']
    cmd = os.path.join(cdir,'solvers-aciss')
    #buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + logfile  + ' \n'
    #buf += 'mpiexec -np 1 ' + cmd + ' '.join(opts) + ' > ' + logfile  + ' \n' 
    buf += cmd + ' '.join(opts) + ' > ' + logfile  + ' \n' 
    print cmd + ' ' + ' '.join(opts)

    pbsscriptfile = '%s/timing-aciss/.timing_%s_%s.sh' % (wdir,matname,hashnum)
    f = open(pbsscriptfile,'w')
    f.write(buf)
    f.close()
    qsubcmd='qsub -q short -N "%s" -l walltime=4:00:00 %s' % (pbsscriptfile,pbsscriptfile)
    ts = time.time()
    ts = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
    print ts, '\n', qsubcmd
    os.system(qsubcmd)

 
