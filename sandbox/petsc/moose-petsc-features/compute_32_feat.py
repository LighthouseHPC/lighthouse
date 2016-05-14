import os,csv

folder = "/home8/kanika/moose/mat_files/"
output = '/home8/kanika/petsc2/src/ksp/ksp/examples/tutorials/moose_features/tutorials/script.sh'
target = open(output,'w')
target.write("clear \n")

feat_file = open('/home8/kanika/petsc2/src/ksp/ksp/examples/tutorials/moose_features/tutorials/results.csv','w')
#write the header to the new decoupled file before writing the feature values
feat_file.write("MinNonzerosPerRow" + "," + "RowVariance" +  "," + "ColumnVariance" + "," + 
"DiagonalVariance" + ","+ "Nonzeros " + ","+ "Dimension" + ","+ "FrobeniusNorm " + ","+ 
"SymmetricFrobeniusNorm " + ","+ "AntiSymmetricFrobeniusNorm" + "," + "OneNorm" + "," + 
"InfinityNorm" + ","+ " SymmetricInfinityNorm" + ","+ "AntiSymmetricInfinityNorm" + ","+
 "MaxNonzerosPerRow " + ","+ "Trace " + ","+ " AbsoluteTrace" + ","+ "MinNonzerosPerRow" + 
 ","+ "AvgNonzerosPerRow" + ","+ "DummyRows" + ","+ "DummyRowsKind" + ","+ "NumericValueSymmetryV1" + 
 ","+ "NonZeroPatternSymmetryV1" + "," + "NumericValueSymmetryV2" + ","+ "NonZeroPatternSymmetryV2" + 
 ","+ "RowDiagonalDominance " + ","+ "ColumnDiagonalDominance " + ","+ "DiagonalAverage" + ","+ 
 "DiagonalSign " + ","+ "DiagonalNonZeros " + ","+ "lowerBandwidth " + ","+ "upperBandwidth " + 
 ","+ "MatrixSymmetric " + "," +"matrix"+ "\n")

for mat_file in os.listdir(folder):
	target.write("./ex100 -f " + folder + mat_file + " >> results.csv" + "\n") 

target.write("exit 0")
