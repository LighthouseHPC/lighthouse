# Usage for f in ~/lustre/12_redo_all/* ; do python parse2csv.py $f ~/tpetra/12_redo_all ; done

import re,sys

input_file = sys.argv[1]
#results_file = open(sys.argv[2] + '_results.csv', 'a')
results_file = open('results.csv', 'a')
reading = open(input_file, 'r')

results_line = ""

for i, line in enumerate(reading):
	if line.find('converge') >= 0:
		results_file.write(line)
reading.close()
results_file.close()
