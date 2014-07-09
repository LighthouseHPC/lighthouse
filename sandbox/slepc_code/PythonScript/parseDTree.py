#!/usr/bin/python

# Code to flatten a decision tree into sql data base files
# Reads file DTreeComplete.txt
# Outputs files slepc_hermitian.sql, slepc_nonhermitian.sql

import re
import copy

# rowid global row position in dtree
rowid=1
CurrentDB = 'lighthouse_slepc_treeLeft';

class Comparison:# an Enumerator
    LT, GTE, E, IN = range(4)

class DataLimits:
	
	def __init__(self):
		self.lowerLimit =0
		self.upperLimit =float(9999999999)
	
	def setLimits(self,comp, value):
		if(comp==Comparison.LT):
			self.upperLimit=value
		elif(comp==Comparison.GTE):
			self.lowerLimit=value
		else:
			print 'Not supposed to be here !'


class slepc_row:
	def __init__(self):
		self.precision = 'd';
		self.isComplex = 0
        	self.size = DataLimits()
		self.numProcessors = DataLimits()
		self.probType=[1,3]
		self.spectrumType = []
		self.nEigenValues = DataLimits()
		self.tolerance=DataLimits()
		self.routineName = 'krylovschur'
		self.binary=0
		self.perfIndex=[]

	def setProperty(self,prop,value, comp=Comparison.E):
		if 'Real' in prop: self.isComplex=int(value);
		elif 'Size' in prop: self.size.setLimits(comp, float(value));
		elif 'Proc' in prop: self.numProcessors.setLimits(comp, float(value));
		elif 'Spectrum' in prop: self.spectrumType = value; 
		elif 'ProbType' in prop: self.probType = value; 
		elif 'NoOfE' in prop: self.nEigenValues.setLimits(comp, float(value));
		elif 'Tol' in prop: self.tolerance.setLimits(comp, float(value));
		elif 'Solver' in prop: self.routineName=value;
		elif 'Bnary' in prop: self.binary=int(value);
		elif 'Rank' in prop: self.perfIndex = value; 

	def printRow(self, outputFile):
		global rowid
		global CurrentDB
		outputFile.write('INSERT INTO {0} VALUES ({1},\'{2}\',\'{3}\',{4},{5},{6},{7},\'{8}\',\'{9}\',{10},{11},{12},{13},\'{14}\',\'{15}\',\'{16}\',\'{17}\',{18});\n'.format(\
		CurrentDB,\
		rowid,\
		self.precision,\
		GetYesNo(self.isComplex),\
		self.size.lowerLimit,self.size.upperLimit,\
		self.numProcessors.lowerLimit,self.numProcessors.upperLimit,\
		",".join(map(str, self.spectrumType)),\
		",".join(map(str, self.probType)),\
		self.nEigenValues.lowerLimit,self.nEigenValues.upperLimit,\
		self.tolerance.lowerLimit,self.tolerance.upperLimit,\
		GetYesNo(self.binary),\
		",".join(map(str, self.perfIndex)),\
		self.routineName,\
		#'www.checkWhichUrl.com',\
		CurrentDB,\
		GetRoutineNumber(self.routineName)
		))
		
		rowid=rowid+1

#This function used just for printing
def GetYesNo(iValue):
	if(iValue): return 'y';
	else: return 'n';

	
	
#This function used just for printing
def GetRoutineNumber(rName):
	if 'power' in rName: return 1;
	elif 'subspace' in rName: return 2;
	elif 'arnoldi' in rName: return 3;
	elif 'lanczos' in rName: return 4; 
	elif 'krylovschur' in rName: return 5;
	elif 'gd' in rName: return 6;
	elif 'jd' in rName: return 7;
	elif 'No' in rName: return 8;

def GetComparatorFromString(propValue):
	if '<' in propValue: return Comparison.LT;
	elif '>=' in propValue: return Comparison.GTE;
	elif '=' in propValue: return Comparison.E;
	elif 'in' in propValue: return Comparison.IN;

# updates the class object
def UpdatePropValue(propValue, row):
	splitObj =re.split('=|<|>=|in', propValue);
	prop= splitObj[0]
	row1=copy.deepcopy(row)
	
	comparator = GetComparatorFromString(propValue);
	if(comparator==Comparison.IN):
		valueBase=re.split('{|}',splitObj[1])
		value=[ int(x) for x in re.split(' ',valueBase[1]) ] 
	else:
		value =splitObj[1];
	row1.setProperty(prop,value, comparator);
	return row1;

# Depth first search creates a row for every leaf node
def DepthFirstSearch(lines, index, row, outputFile):
	line=lines[index];
	if 'class' in line: 
		solver= re.split('=',line)[1].strip();
		#print solver
		row.setProperty('Solver',solver);
		row.printRow(outputFile);
		return;
	obj = re.search(r'if (.*) then node (.*) elseif (.*) then node (.*) else (.*)',line)
	
	# set property value for the left
	propValue1 = obj.group(1)
	node1 = obj.group(2)
	row1 = UpdatePropValue(propValue1,row)
	DepthFirstSearch(lines,int(node1)-1,row1,outputFile);

	# set property value for the right
	propValue2 = obj.group(3)
	node2 = obj.group(4)
	row2 = UpdatePropValue(propValue2,row)
	DepthFirstSearch(lines,int(node2)-1,row2,outputFile);
	

#read file get all lines
Inputfilename='DTreeComplete.txt'
lines = [line.strip() for line in open(Inputfilename)]


#splitting the first line and saving first property
line=lines[0];
obj = re.search(r'if (.*) then node (.*) elseif (.*) then node (.*) else (.*)',line)

###########
# LEFT TREE
###########

#set left database , DB name , filename and start from row 1
rowid=1
row = slepc_row();
outputFile = open("slepc_treeleft.sql", "wb")
CurrentDB = 'lighthouse_slepc_treeleft'

# set property value for the left
propValue1 = obj.group(1)
node1 = obj.group(2)
row1 = UpdatePropValue(propValue1,row)

#run recursive dfs
DepthFirstSearch(lines,1,row1,outputFile);
outputFile.close();

###########
# RIGHT TREE
###########

# set right DB name , filename and start from row 2
rowid=1
row = slepc_row();
outputFile = open("slepc_treeright.sql", "wb")
CurrentDB = 'lighthouse_slepc_treeright'

# set property value for the right
propValue2 = obj.group(3)
node2 = obj.group(4)
row2 = UpdatePropValue(propValue2,row)

#run recursive dfs
DepthFirstSearch(lines,2,row2,outputFile);
outputFile.close();


