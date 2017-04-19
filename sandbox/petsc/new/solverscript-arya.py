#!/usr/bin/env python
# This script requires all matrices to be in PETSc binary format in the petsc subdir

import sys, os, glob, random
import datetime,time

nprocs = [1,4,8,16,24,32,48,54,72]
nprocs = [8,1]

from solvers import *
petsc = True
if petsc:
  tmdir = 'timing-arya'
  matrixsubdir = 'petsc'
  petsc_matrix_suffix='.petsc'
  donefile = 'DONE'
  jobname = 'petsc'
else:
  tmdir = 'timing-moose-aciss'
  matrixsubdir = 'moose'
  petsc_matrix_suffix='.mat'
  donefile = 'DONE_moose'
  jobname = 'moose'
#jobname = 'default'

def getJobs():
  s = commands.getstatusoutput('qstat -a | grep norris | grep %s | wc -l' % jobname)[1]
  return int(s)

# Directory contaning the *.petsc matrices:
#wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
wdir='/disks/large/shared/soft/UFloridaSparseMat/'
# Directory for storing results:
tdir=wdir + tmdir + '/'
mdir=wdir + matrixsubdir + '/'
cdir='/home/users/norris/research/lighthouse/sandbox/petsc/new/' #os.getcwd() 

matrices = glob.glob(mdir+'/*.%s' % petsc_matrix_suffix)
donelist=[]
# DONE_TRILINOS
if os.path.exists(donefile):
  donelist=open(donefile,'r').readlines()
else:
  print "Error: can't find done matrix file"
  exit(1)

import commands
#if (getJobs() > 0):
  #donelist = list(reversed(donelist))

solveropts = getsolvers()

env = os.environ
hashlist = solveropts.keys()
random.shuffle(hashlist)
print hashlist

for np in nprocs:
  for hashnum in hashlist:
    print "Hi", tdir, hashnum, np
    solver_optstr = solveropts[hashnum]
    #if totalprocs > nprocs: break
    for matname in donelist:
      matname = matname.strip()
      matrixpath=mdir+matname+petsc_matrix_suffix
      if not os.path.exists(matrixpath):
        print "No PETSc matrix:", matrixpath
        continue
      else:
        print "PETSc matrix:", matrixpath
      logfile = tdir + '%s.%s.p%d.log' % (matname, str(hashnum), np)
      lockfile = tdir + '.%s.%s.p%d.log' % (matname, str(hashnum), np)
      print "Logfile:", logfile
      if os.path.exists(lockfile) or os.path.exists(logfile): continue
      else: os.system("echo %s > %s" % (matname,lockfile))
      opts = [' -f ',mdir+matname+petsc_matrix_suffix, ' -hash', hashnum, solver_optstr, ' -logfile', logfile, ' -ksp_view -options_left -ksp_error_if_not_converged 1 -ksp_converged_reason ']
      cmd = os.path.join(cdir,'solvers-arya')
      #buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + logfile  + ' \n'
      buf = 'mpiexec -np %d ' %np  + cmd + ' '.join(opts) + ' \n' 
      buf += cmd + ' '.join(opts) +  '; \n' 
     
  
      #pbsscriptfile = '%s/.timing_%s_%s.sh' % (tdir,matname,str(hashnum))
      #f = open(pbsscriptfile,'w')
      #f.write(buf)
      #f.close()
      #qsubcmd='qsub -q short -N "%s" -l walltime=4:00:00 %s' % (jobname,pbsscriptfile)
      ts = time.time()
      ts = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
      print ts, '\n', buf
      os.system(buf)
  
   
