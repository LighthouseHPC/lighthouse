#!/usr/bin/env python
# This script requires all matrices to be in PETSc binary format in the petsc subdir

import sys, os, glob, random
import datetime,time
import Command

#nprocs = [1,4,40,78,80]
nprocs = [38]
TIMEOUT=300

from solvers import *
petsc = True
if petsc:
  tmdir = 'timing-artemis-p38'
  matrixsubdir = 'petsc'
  petsc_matrix_suffix='.petsc'
  donefile = 'DONE'
  jobname = 'petsc'
else:
  tmdir = 'timing-moose-artemis-p38'
  matrixsubdir = 'moose100'
  petsc_matrix_suffix='.mat'
  donefile = 'DONE_moose'
  jobname = 'moose'
#jobname = 'default'

def getJobs():
  s = commands.getstatusoutput('qstat -a | grep norris | grep %s | wc -l' % jobname)[1]
  return int(s)

#dDirectory contaning the *.petsc matrices:
#wdir='/gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/'
wdir='/shared/data/sparse-matrices/'
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

#solveropts, solverstring = getsolvers()
solveropts = getsolvers()

env = os.environ
hashlist = solveropts.keys()
random.shuffle(hashlist)
print hashlist

for np in nprocs:
  for hashnum in hashlist:
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
      cmd = os.path.join(cdir,'solvers-artemis')
      #buf += 'runjob --np 1 -p ' + str(p) + ' --block $COBALT_PARTNAME --verbose=INFO : ' + cmd + ' ' + ' '.join(opts) + ' > ' + logfile  + ' \n'
      buf = 'mpiexec -np %d ' %np  + cmd + ' '.join(opts) + ' \n' 
     
  
      #pbsscriptfile = '%s/.timing_%s_%s.sh' % (tdir,matname,str(hashnum))
      #f = open(pbsscriptfile,'w')
      #f.write(buf)
      #f.close()
      #qsubcmd='qsub -q short -N "%s" -l walltime=4:00:00 %s' % (jobname,pbsscriptfile)
      ts = time.time()
      ts = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
      print ts, '\n', buf
      #Hash: nohash
      #ibcgs | jacobi | reason=2 | time=2.266693e-02 | norm=0.000477256 | its=6 | p=1
      #failstring = 
      failstring='TIMED OUT:' + ' '.join(opts) + '\n' + buf + '\n'

      retcode, out, err = Command.Command(buf).run(timeout=TIMEOUT,failstring=failstring)
      print 'Errors:',err
      #print ' | '.join(['TIMED OUT:',matname,hashnum,failstring])
  
   
