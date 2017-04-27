# author : Kanika Sood 
# Date : April 20, 2017


import csv
from collections import OrderedDict
from itertools import islice 
import operator

class SolverDetails:
	nprocs = []
	times = []
	base_proc = 1
	base_time = 0.1

nprocs_per_solver = {} 

solvernames= ['cg/jacobi','cg/bjacobi','cg/asm',
'gmres/bjacobi','gmres/asm', 'gmres/jacobi', 
'fgmres/bjacobi', 'fgmres/asm', 'fgmres/jacobi', 
'tfqmr/jacobi', 'tfqmr/bjacobi' ,'tfqmr/asm'
'bicg/asm','bicg/jacobi', 'bicg/bjacobi']

with open('Scalability.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		csvoutput = 'Scalability-output.csv'
		with open(csvoutput,'w') as csvoutput:
			writer = csv.writer(csvoutput)
			header= infile.__next__() 
			num_procs = []
			writer.writerow(header + ['speedup'] + ['efficiency']) #write the header to the new file before writing the feature values
			for row in islice(infile, 1, None):
				solverName = str(row[2])
				nproc = int(row[0])
				time = float(row[3])				

				if solverName not in nprocs_per_solver: 
						solver_details = SolverDetails()
						solver_details.base_time = time
						solver_details.base_proc = nproc
						nprocs_per_solver[solverName] =solver_details

				else:
					print("In....", solverName)
					solver_details = nprocs_per_solver[solverName]

		
				speedup = solver_details.base_time/time 
				efficiency = speedup / (nproc/solver_details.base_proc)	* 100
				print('Speedup: ', speedup)
				print('Efficiency', efficiency)
				print(solverName, time, nproc, solver_details.base_time, solver_details.base_proc, speedup, efficiency)
						
				writer.writerow(row + [speedup] + [efficiency])

