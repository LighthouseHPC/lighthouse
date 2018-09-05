#!/usr/bin/env python3

'''
Date: August 22, 2018
Project: Matrix-free feature computation for UFlorida data on arya

To run: python3 write_UF_features_to_csv.py
Input: .out files (1 for each matrix)
Output: A single csv file with all the features
'''

import csv
import os


def read_out_files(location, csv_filename):
	header_written = False
	with open(csv_filename, 'w') as csvfile:
		csvwriter = csv.writer(csvfile)
		for out_file in os.listdir(location):
			if out_file.endswith('.out'):
				print(out_file)
				infile = open(location + '/' + out_file, 'r')
				lines = infile.readlines()
				print(lines)
				if not header_written:
					print("line 0" +lines[0])
					csvwriter.writerow([lines[0]])
					header_written = True
				print('-------')
				csvwriter.writerow([lines[1]])
	csvfile.close()
	clean_csv(csv_filename)

def clean_csv(csv_filename):
	cleaned_csv_file = 'cleaned_' + csv_filename
	# 1. Remove ^M character from csv 
	# 2. Remove quotes from csv
	# 3. Split columns based on ,
	# 4. Edit Matrix name column to exclude location of matrix. Sample: abc.mat
	with open(cleaned_csv_file, 'w') as csvfile: 
		csvwriter = csv.writer(csvfile)
		with open (csv_filename, 'r') as new_csv:
			for line in new_csv:
				line = line.replace('^M', '')
				line = line.replace('"', '')
				cols = line.split(',')
				cols[0] = cols[0].split('/')[-1]
				cols[-1] = cols[-1].split('\n')[0]
				if line == '\r\n': #remove blank lines from csv
					pass
				else:
					mname = cols[0].split('/')[-1]
					features = ','.join(str(e) for e in cols[0:len(cols)+2]).split(',')
					features = str(features).replace('[','').replace(']','')
					features = str(features).replace("\'", "").replace('\"', '')
					row = features
					row = str(row).replace("\'", "").replace('\"', '').replace(' ', '').replace('\n', '').replace(',,', ',').replace('[','').replace(']','')
					csvwriter.writerow([row])
					print(row)
	print('Written features in file: ', cleaned_csv_file)

location='/home/users/kanikas/RNET_git/features_extracted_UF'
read_out_files(location, 'UF_features_mfree.csv')

