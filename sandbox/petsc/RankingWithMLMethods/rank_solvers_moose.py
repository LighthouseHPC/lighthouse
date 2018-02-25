#!/usr/bin/env python3

'''
Date: February 23, 2018
Author: Kanika Sood

Input: Property csv file with matrix name + matrix properties + time + class label
Output: Csv file with solver ranking based on the solver time for each LS 

Usage: python3 rank_solvers_moose.py -f filename_with_its_path
Example: python3 rank_solvers_moose.py -f filtered_moose.csv
Prerequisites: input csv is sorted first by matrix_name then by time
'''

import csv, sys, argparse
from itertools import islice


class RankSolvers():

	def rankSolvers(self, filename):
		f = open(filename, 'r')
		num_rows = len(f.readlines())
		f = open(filename, 'r')
		infile = csv.reader(f)
		outfile = 'properties_moose_with_mname_ranking.csv'
		rank = 0
		last = '' #first row
		last_time = 0.0
		with open(outfile, 'w+') as csvoutput: 
			writer = csv.writer(csvoutput)
			header = next(infile) 
			writer.writerow(header + ['rank'])
			for row in islice(infile, 1, None):
				if last == '':
						rank = 1
				else:
					if row[36] == last: 
						if float(row[34]) != float(last_time):  # if same time for 2 solvers then dont change the rank
							rank += 1
					else: 
						rank = 1
				last = row[36]
				last_time = row[34]
				writer.writerow(row + [rank])
		print('Done.... Solver ranks written in file: ', outfile)
		f.close()
		csvoutput.close()

if __name__ == '__main__': 
	parser = argparse.ArgumentParser()
	parser.add_argument('-f', '--filename', help= 'Please enter the name of the csv file with the property', type=str)
	args = parser.parse_args()
	in_file = 'filtered_moose.csv'

	ranking = RankSolvers()
	ranking.filename = args.filename
	ranking.rankSolvers(in_file)
