# author : Kanika Sood 
# Date : Jan 13

#input file: '/Users/kanikas/Desktop/petsc_anamod_35.csv' (File has all features + solver + class)
#output file: '/Users/kanikas/Desktop/solver_pc.csv' (File has all features + solver + solver_name + pc_name + class ) manually removing solver from the list for now
import csv
from collections import OrderedDict
from itertools import islice 
import operator
solverCount = {}
ksp_pc = {}

solvers = ['gmres','fgmres','lgmres','bicg','bcgs','tmfqmr','tcqmr','lsqr','chebychev','cg', 'ibcgs']
pcs = { 'ilu': {'factor_levels':[0,1,2,3]},
        'asm': {'asm_overlap' : [0,1,2,3]},
        'jacobi' : {},
        'bjacobi' : {},
        'icc' : {'factor_levels':[0,1,2,3]}
    }

#solverids only for reference , 154 in count
# solverids = ['89565283','8793455','90197667','49598909','91036839','45869639','45869638','45869637',
# '47942867','89269802','89269803','89269801','89269804','59072883','59072882','59072881','7285381','7285384',
# '59072884','49598911','49598910','49598912','30870721','36025723','36025722','53302993','30870720',
# '64278029','36025724','44526267','8793454','8793456','8793453','17734818','32168839','32168838','57331597',
# '95762352','57331599','57331598','32168837','88865078','88865079','49834417','49834419','49834418',
# '88865076','88865077','11256942','11256943','11256941','42851841','11256944','31459546','17887723',
# '91845162','53362206','95762355','12321508','75830644','57331600','43373444','95762353','43373441',
# '43373442','43373443','91068411','91068410','5890861','85483012','5890863','5890862','18868444','18868441',
# '18868443','18868442','91068408','47942864','47942865','47942866','91068409','8520536','5890860','82456576',
# '29030069','95762354','90783920','99720138','29030071','29030070','29030072','85490469','30870723','30870722',
# '26415435','26415434','26415433','26415432','13323659','45869640','36564233','68908713','7285382',
# '36564232','7285383','75830645','36564234','81986705','29553941','29553943','29553942','69654761',
# '29553944','69654763','69654762','32874609','32168840','90197664','90197665','90197666','69654760',
# '37052870','37052871','19932321','19932323','19932322','19932324','80361466','80361467','80361464',
# '80361465','49834420','1216556','38678404','38678401','38678402','38678403','32874611','32874610',
# '32874612','44114477','44114476','44114479','44114478','36564235','36025721','75830647','75830646',
#'85490471','85490470','85490472','18524981','37052869','37052868']

#with open('/Users/kanikas/Desktop/petsc_anamod_aciss_1.csv', 'r') as csvinput:
#with open('/Users/kanikas/Desktop/petsc_anamod_35.csv', 'r') as csvinput:
with open('/Users/kanikas/Documents/research/ResultsSolverPCSeparate/bgq/petsc_anamod_bgq_35.csv', 'r') as csvinput:
	with open('/Users/kanikas/Desktop/solver_pc.csv', 'w') as csvoutput:
		writer = csv.writer(csvoutput)
		for row in csv.reader(csvinput):
			writer.writerow(row + ['solver_name'] + ['pc_name'])
			
#Open and read the solver unique numbers from solverids_names.csv and make them the key of the dictionary
uniques_ids = {}
solverids_names = csv.reader(open('/Users/kanikas/Desktop/solverids_names.csv', 'r'))
names = []
names_solvers = {}
for name in islice(solverids_names, 1, None): # to slice the column name while reading the csv
#for name in solverids_names:
	key = name[0]
	names.append(key)
	names_solvers[key] = [name[1], name[2]] # unique solver_id is the key and value is (sname, pcname)
#print(names_solvers, len(names_solvers), type(names_solvers)) #names list has all the 154 solvers
# to access solvername use: names_solvers['26415432'][0] and for pcname: names_solvers['26415432'][1]

	#adds sname and pcname to the csv file and removes the solver id column from the file
with open('/Users/kanikas/Desktop/solver_pc.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		with open('/Users/kanikas/Documents/research/ResultsSolverPCSeparate/bgq/decoupled/solver_pc_separate_bgq_35.csv', 'w+') as csvoutput:
			writer = csv.writer(csvoutput)
			for row in islice(infile, 1, None):
				for key in names_solvers.keys():
					if row[68] == key :
						print(row[68],  key) #error line  step 2 implement
						row[70] = names_solvers[row[68]][0]
						row[71] = names_solvers[row[68]][1]
						print(row[70], row[71])
						writer.writerow(row)

#write the result(solvers + no. of occurrences) to output file
outFile = open('solver_pc_separate.txt', 'w')
outFile.seek(0)
outFile.write("(solver name, pc name) \n")

 		