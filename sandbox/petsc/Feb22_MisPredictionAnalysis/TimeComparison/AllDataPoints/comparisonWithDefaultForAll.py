# author : Kanika Sood 
# Date : Feb 18

# default timings available for 775 matrices (default solver: gmres + ilu , solver id: 32168839)
#input file: '/Users/kanikas/Desktop/petsc_anamod_35.csv' (File has all features + solver + class)
#output file: '/Users/kanikas/Desktop/solver_pc.csv' (File has all features + solver + solver_name + pc_name + class ) manually removing solver from the list for now
import csv
from collections import OrderedDict
from itertools import islice 
import operator


			
#Open and read the solver unique numbers from solverids_names.csv and make them the key of the dictionary
uniques_ids = {}
default_time = {}

with open('/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/TimeComparison/AllDataPoints/allDataPoints.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		csvoutput = '/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/TimeComparison/AllDataPoints/default_Time.csv'
		with open(csvoutput,'w') as csvoutput:
			writer = csv.writer(csvoutput)
			header= infile.__next__() 
			#print(header)
			writer.writerow(header + ['default_time']) #write the header to the new decoupled file before writing the feature values
			for row in islice(infile, 1, None):
					if row[67] == "32168839" :
						default_time[row[68]] = row[69]
					writer.writerow(row)

with open('/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/TimeComparison/AllDataPoints/default_Time.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		csvoutput = '/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/TimeComparison/AllDataPoints/TimeComparisonWithDefaultAll.csv'
		with open(csvoutput,'w') as csvout:
			writer = csv.writer(csvout)
			infile = csv.reader(csvinput)
			writer.writerow(header + ['default_time'] + ['Time Difference'])
			diff = 0.0
			bad=0
			good=0
			print(len(default_time))
			for row in infile:
				for key in default_time:
					if row[68] == key  :
						#print(key)
						diff = round(((float(row[69]) - float(default_time[key]))/float(default_time[key]))*100,2)
						if diff>0:
							bad+=1
							ext = " % more time of default solver time"
						else:
							good+=1
							ext = " % less time of default solver time"
						diff = str(diff) + ext

						writer.writerow(row + [default_time[key]] + [diff])
		print("We did better than default solver in ",good,"many cases out of ",(good+bad))
		print(good,bad, (good+bad))
		print("Result in file -->", csvoutput)


 		