# author : Kanika Sood 
# Date : May 1, 2017
#Input that should be in the same directory: edison-ex19-kspsolve_filtered_final.csv

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


def write_files_per_problem():
	solvers = ['gmres','fgmres','bicg','bcgs','tfqmr','cg', 'ibcgs'] # 7 solvers
	pcs = { 
	        'asm': {'asm_overlap' : [0,1,2,3]}, # 7 pc+optns 
	        'jacobi' : {}, 
	        'bjacobi' : {}, 
	        'none' : {}
	    }
	solver_pcs = []
	combination = "" #total 49 combinations

	for solver in solvers:
		for pc, pcoptns in pcs.items():
			if len(pcoptns) == 0:
				combination = solver + "/" + pc 
				solver_pcs.append(combination)
			else: 									# asm case
				for item in pcoptns['asm_overlap']:
					combination = solver + "/" + pc + "/overlap" + str(item)
					solver_pcs.append(combination)

	print(solver_pcs, len(solver_pcs)) #49

	#problems = ['s1000_g1e2', 's1000_g1e3', 's1000_g1e4', 's1000_g1e5', 's20_g1e2', 's20_g1', 's1000_g1'] #p1,p2,p3,p4,p5,p6, p7, p8
	matrices = set()

	with open('edison-ex19-kspsolve_filtered_final.csv', 'r+') as csvinput:
			infile = csv.reader(csvinput)
			header = infile.__next__() 
			for row in islice(infile, 1, None):
				matrices.add(row[0])
	problems = list(matrices)
	problems.sort()
	print(problems, len(problems))
	out_files = []
	for i in range (0,len(problems)):  # loop over all problems
		with open('edison-ex19-kspsolve_filtered_final.csv', 'r+') as csvinput:
			infile = csv.reader(csvinput)
			header = infile.__next__()	
			print(i)
			out_file = 'csvoutput' + str(i+1)
			out_file = 'edison-ex19_filtered_' + problems[i] + '_p' + str(i+1) + '_v2.csv'
			filename = 'edison-ex19_filtered_' + problems[i] + '_p' + str(i+1) + '_v2.csv'
			out_file2 = 'edison-ex19_Scalability-output_' + problems[i]  + '_p' + str(i+1) + '_v2.csv'
			out_files.append(out_file)
			
			with open(out_file,'w') as out_file:
				writer = csv.writer(out_file)	
				writer.writerow(header)
				for row in islice(infile, 1, None):

					for solver in solver_pcs:
						if str(row[3]) == solver and str(row[4]) == 'KSPSolve' and problems[i] == str(row[0]):
							writer.writerow(row)
		efficiency_compute(filename, out_file2)

def efficiency_compute(in_file, out_file):
	in_file_sorted = in_file.split('.')[0] + '_sorted.csv'
	with open(in_file, 'r+') as csvinput, open(in_file_sorted, 'w') as csvinput_sorted: # reading the i/p file and sorting it based on nprocs
			infile = csv.reader(csvinput)
			writer_sorted = csv.writer(csvinput_sorted)
			header1 = next(infile) #pop header out
			writer_sorted.writerow(header1)
			sorted_list = sorted(infile, key=lambda row: int(row[1])) #sort based on nprocs to have the lowest count on top of the file
			writer_sorted.writerows(sorted_list)
			csvoutput = out_file
	with open(csvoutput,'w') as csvoutput, open(in_file_sorted,'r+') as csvinput_sorted:
				writer = csv.writer(csvoutput)
				infile_sorted = csv.reader(csvinput_sorted)
				header= infile_sorted.__next__() 
				num_procs = []
				writer.writerow(header1 + ['speedup'] + ['efficiency']) #write the header to the new file before writing the feature values
				for row in islice(infile_sorted, 1, None):
					solverName = str(row[3])
					nproc = int(row[1])
					time = float(row[7]) 
					print(solverName, nproc)
			
					#first entry for a solver becomes the base case
					if solverName not in nprocs_per_solver or nproc == '1': 
							solver_details = SolverDetails() #creating the class object
							solver_details.base_time = time #base time: time of the first entry: in most cases it is nprocs: 24 or the least np count
							print(solver_details.base_time)
							solver_details.base_proc = nproc
							nprocs_per_solver[solverName] = solver_details
					else:
						print("In....", solverName)
						solver_details = nprocs_per_solver[solverName]
			
					speedup = time/solver_details.base_time
					efficiency = speedup / (nproc/solver_details.base_proc)	* 100
					print('Speedup: ', speedup)
					print('Efficiency', efficiency)
					print(solverName, time, nproc, solver_details.base_time, solver_details.base_proc, speedup, efficiency)						
					writer.writerow(row +  [speedup] + [efficiency])

if __name__ == "__main__":

	write_files_per_problem()
	
	# efficiency_compute('edison-ex19_filtered_s1000_g1_p1_v2.csv', 'edison-ex19_Scalability-output_s1000_g1_p1_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s1000_g10_p2_v2.csv', 'edison-ex19_Scalability-output_s1000_g10_p2_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s1000_g1e2_p3_v2.csv', 'edison-ex19_Scalability-output_s1000_g1e2_p3_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s1000_g1e3_p4_v2.csv', 'edison-ex19_Scalability-output_s1000_g1e3_p4_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s1000_g1e4_p5_v2.csv', 'edison-ex19_Scalability-output_s1000_g1e4_p5_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s1000_g1e5_p6_v2.csv', 'edison-ex19_Scalability-output_s1000_g1e5_p6_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s20_g1_p7_v2.csv', 'edison-ex19_Scalability-output_s20_g1_p7_v2.csv')
	# efficiency_compute('edison-ex19_filtered_s20_g1e2_p8_v2.csv', 'edison-ex19_Scalability-output_s20_g1e2_p8_v2.csv')

