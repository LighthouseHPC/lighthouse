import os,csv
folder = "/home8/kanika/moose/mat_files/"
output = '/home8/kanika/petsc2/src/ksp/ksp/examples/tutorials/moose_features/tutorials/script.sh'

target = open(output,'w')
target.write("clear \n")
                        #write the header to the new decoupled file before writing the feature values
target1 = open('/home8/kanika/petsc2/src/ksp/ksp/examples/tutorials/moose_features/tutorials/results.csv','w')

target1.write("MinNonzerosPerRow" + "," + "RowVariance" +  "," + "ColumnVariance" + "," + 
"DiagonalVariance" + ","+ "Nonzeros " + ","+ "Dimension" + ","+ "FrobeniusNorm " + ","+ 
"SymmetricFrobeniusNorm " + ","+ "AntiSymmetricFrobeniusNorm" + "," + "OneNorm" + "," + 
"InfinityNorm" + ","+ " SymmetricInfinityNorm" + ","+ "AntiSymmetricInfinityNorm" + ","+
 "MaxNonzerosPerRow " + ","+ "Trace " + ","+ " AbsoluteTrace" + ","+ "MinNonzerosPerRow" + 
 ","+ "AvgNonzerosPerRow" + ","+ "DummyRows" + ","+ "DummyRowsKind" + ","+ "NumericValueSymmetryV1" + 
 ","+ "NonZeroPatternSymmetryV1" + "," + "NumericValueSymmetryV2" + ","+ "NonZeroPatternSymmetryV2" + 
 ","+ "RowDiagonalDominance " + ","+ "ColumnDiagonalDominance " + ","+ "DiagonalAverage" + ","+ 
 "DiagonalSign " + ","+ "DiagonalNonZeros " + ","+ "lowerBandwidth " + ","+ "upperBandwidth " + 
 ","+ "MatrixSymmetric " + "," +"matrix"+ "\n")

for file1 in os.listdir(folder):
				target.write("./ex100 -f " + folder + file1 + " >> results.csv" + "\n") 

target.write("exit 0")