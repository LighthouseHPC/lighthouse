#!/usr/bin/env python

import sys

#KSP: fgmres, lgmres, gmres, bcgs, bicg, tfqmr, tcqmr, lsqr
#ymmetric only: chebyshev, cg
#arallel only: ibcgs

#PC: sor, jacobi, bjacobi, asm(k) [k=0..3], 
#sequential only: ilu(k) [k=0..3], 
#symmetric only: icc(k) instead of ilu
#any matrix with (some) 0 diagonal entries:
#-pc_fieldsplit_type schur in conjunction with -pc_fieldsplit_detect_saddle_point (Boyana will look into that)

matrix_free = True
solvers = ['gmres','fgmres','lgmres','bicg','bcgs','tfqmr','tcqmr','lsqr','chebyshev','cg', 'ibcgs']
pcs = { 'ilu': {'factor_levels':[0,1,2,3]},
	'bjacobi' : {},
	'asm': {'asm_overlap' : [0,1,2,3]},
     	'jacobi' : {},
	'icc' : {'factor_levels':[0,1,2,3]}
    }
if True:    # Parallel solvers 
  solvers = ['gmres','preonly','fgmres','dgmres','cg','fcg', 'tfqmr','hypre','chebyshev','gcr','lcd','lgmres','pipefgmres','pipefcg','richardson']
  if not matrix_free: solvers += 'bicg'
  pcs = {
        'none' : {},
        'hypre' : {'pilut':[],'boomeramg':[],'parasails':[]},
        'preonly' : {'lu':[],'cholesky':[]},
        'asm': {'asm_overlap' : [0,1,2,3]},
        'gasm' : {'gasm_overlap' : [0,1,2,3]},
        'mg' : {'mg_levels' : [1,2,3], 'mg_cycle_type':['v','w']},
        'jacobi' : {},
        'bjacobi' : {},
        'sor' : {},
        'svd' : {},
  }


# Default, comment out for general random sampling testing
#if len(sys.argv) > 1 and sys.argv[1] == 'default':
if False:
  solvers = ['gmres']
  pcs = {'ilu' : {'factor_levels':[0]}}

def genhash(somestr):
  return  str(abs(hash(somestr.strip())) % (10 ** 8))

def getsolvers(which='petsc'):
  solveropts = {}
	
  for solver in solvers:
    for pc, pcopts in pcs.items():
      if len(pcopts) == 0:
        optstr = ' -ksp_type %s -pc_type %s ' % (solver, pc)
        hashstr = genhash(optstr)
        solveropts[hashstr] = optstr
        continue
      for pc_optname, pc_optvals in pcopts.items():
        for pc_optval in pc_optvals:
          optstr = ' -ksp_type %s -pc_type %s -pc_%s %s ' % (solver, pc, pc_optname, str(pc_optval))
          hashstr = genhash(optstr)
          solveropts[hashstr] = optstr
  return solveropts

if __name__ == "__main__":
  solveropts = getsolvers()
  #print solveropts
  for key,val in solveropts.items():
    print key,',', val
  print "Total number of combinations:", len(solveropts.keys())

