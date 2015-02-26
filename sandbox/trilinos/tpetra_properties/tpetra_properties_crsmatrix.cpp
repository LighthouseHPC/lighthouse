#include "tpetra_properties_crsmatrix.h"

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int numNodes;

int main(int argc, char *argv[]) {
	
	//  General setup for Teuchos/communication
	Teuchos::GlobalMPISession mpiSession(&argc, &argv);
	Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
	comm = rcp (new Teuchos::MpiComm<int> (MPI_COMM_WORLD));	
	RCP<NT> node = platform.getNode();
	int myRank = comm->getRank();
	Teuchos::oblackholestream blackhole;
	std::ostream& out = (myRank == 0) ? std::cout : blackhole;
	fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));

	// Load and run tests on Matrix Market file
	std::string filename("../ecl32.mtx");
	RCP<MAT> A = Tpetra::MatrixMarket::Reader<MAT>::readSparseFile(filename, comm, node, true);
	runGauntlet(A);

}
void runGauntlet(const RCP<MAT> &A) {
	// Test squareness
	if (A->getGlobalNumRows() != A->getGlobalNumCols() ) {
		*fos << "Not a square matrix, exiting\n";
		exit(-1);
	}
	numNodes = comm->getSize();
	*fos << "nodes:" << numNodes << " nodes" << std::endl;

// Working
	calcRowVariance(A);
	calcColVariance(A);
	calcDiagVariance(A);
	calcNonzeros(A);
	calcDim(A);
	calcFrobeniusNorm(A);
	calcSymmetricFrobeniusNorm(A);
	calcAntisymmetricFrobeniusNorm(A);
	calcOneNorm(A);
	calcInfNorm(A, false);
	calcSymmetricInfNorm(A);
	calcAntisymmetricInfNorm(A);
	calcMaxNonzerosPerRow(A);
	calcMinNonzerosPerRow(A);
	calcAvgNonzerosPerRow(A);

// not checked
	calcTrace(A, false);
	calcAbsTrace(A);
	calcDummyRows(A);
	//calcDummyRowsKind(A);
	calcNumericalSymmetry(A);
	calcNonzeroPatternSymmetry(A);
	//calcNumericalValueSymmetry(A);
  //calcNonzeroPatternSymmetry(A);
	calcRowDiagonalDominance(A);
	calcColDiagonalDominance(A);

	//calcDiagonalSign(A);
	//calcDiagonalNonzeros(A);
	calcLowerBandwidth(A);
	//calcUpperBandwidth(A);
}

//  Return the maximum row locVariance for the matrix
//  The average of the squared differences from the Mean.
void calcRowVariance(const RCP<MAT> &A, bool transpose) {
	LO rows = A->getGlobalNumRows(); 
	ST mean, locVariance, locMaxVariance, result = 0.0;
	RCP<const MAP> m = A->getRowMap();

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			mean = locVariance = 0.0; 
			size_t cols = A->getNumEntriesInGlobalRow(row); 
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols); 

		//  Two-step approach for locVariance, could be more efficient 
			for (LO col = 0; col < cols; col++) {
				mean += values[col];
			} 
		//  Divide entries by the dim (to include zeros)
			mean /= A->getGlobalNumCols();
			for (LO col = 0; col < cols; col++) {
				locVariance += (values[col] - mean) * (values[col] - mean);
			}
			locVariance /= A->getGlobalNumCols();
			if (locVariance > locMaxVariance) locMaxVariance = locVariance;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxVariance, &result);
	if (transpose == false) {
		*fos << "row variance:" << result << std::endl;
	} else {
		*fos << "col variance:" << result << std::endl;
	}
}

//  Transpose the matrix, get row locVariance 
void calcColVariance(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
	calcRowVariance(B, true);
}

//  The variance of the diagonal
void calcDiagVariance(const RCP<MAT> &A) {
	ST locMean, mean, result, locVariance = 0.0; 
	typedef Tpetra::Map<LO, GO> map_type; 
	GO numGlobalElements = A->getGlobalNumDiags(); 
	RCP<const map_type> map = rcp(new map_type (numGlobalElements, 0, comm)); 
	VEC v(map);

	A->getLocalDiagCopy(v);
	Teuchos::ArrayRCP<const ST> array = v.getData();	
	for (int i = 0; i < array.size(); i++) {
		locMean += array[i];	
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locMean, &mean);
	mean /= A->getGlobalNumRows();
	for (int i = 0; i < array.size(); i++) {
		locVariance += (array[i] - mean) * (array[i] - mean);
	}	
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locVariance, &result);
	result /= A->getGlobalNumRows();
	*fos << "diag variance:" << result << std::endl;
}

//  Total number of nonzeros in matrix
void calcNonzeros(const RCP<MAT> &A) {
	*fos << "nonzeros:" << A->getGlobalNumEntries() << ", " << std::endl;
}

//  Dimension of the square matrix
void calcDim(const RCP<MAT> &A) {
	*fos << "dimension:" << A->getGlobalNumRows() << ", " << std::endl;
}

//  Frobenius norm of matrix
void calcFrobeniusNorm(const RCP<MAT> &A) {
	*fos << "frob norm:" << A->getFrobeniusNorm() << ", " << std::endl;
}

//  Symmetric A_s = (A+A')/2
void calcSymmetricFrobeniusNorm(const RCP<MAT> &A){ 
	RCP<MAT> A_s = Tpetra::MatrixMatrix::add(0.5, false, *A, 0.5, true, *A);
	*fos << "symm frob norm:" << A_s->getFrobeniusNorm() << ", " << std::endl;
}

//  Antisymmetric A_a = (A-A')/2
void calcAntisymmetricFrobeniusNorm(const RCP<MAT> &A){ 
	RCP<MAT> A_a = Tpetra::MatrixMatrix::add(0.5, false, *A, -0.5, true, *A);
	*fos << "antisymm frob norm:" << A_a->getFrobeniusNorm() << ", " << std::endl;
}

//  Max absolute row sum
void calcInfNorm(const RCP<MAT> &A, bool transpose) {
	LO localRows = A->getNodeNumRows(); 
	ArrayView<const ST> values;
	ArrayView<const LO> indices;
	GO numColsInCurrentRow;
	ST locSum, locMaxSum, result = 0.0;

	//  Go through each row on the current process
	for (LO row = 0; row < localRows; row++) {
		locSum = 0.0;
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			numColsInCurrentRow = A->getNumEntriesInLocalRow(row); 
			A->getLocalRowView(row, indices, values); 

			for (LO col = 0; col < numColsInCurrentRow; col++) {
				locSum += fabs(values[col]);
			} 
			if (locSum > locMaxSum) locMaxSum = locSum;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxSum, &result);
	if (transpose) {
		*fos << "one norm:" << result << std::endl;
	} else {
		*fos << "inf norm:" << result << std::endl;
	}
}

//  Max absolute column sum
void calcOneNorm(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
	calcInfNorm(B, true);
}

//  Max absolute row sum of symmetric part
void calcSymmetricInfNorm(const RCP<MAT> &A) {
	RCP<MAT> A_s = Tpetra::MatrixMatrix::add(0.5, false, *A, 0.5, true, *A);
	*fos << "symmetric ";
	calcInfNorm(A_s, false);
}

//  Max absolute row sum of anti-symmetric part
void calcAntisymmetricInfNorm(const RCP<MAT> &A) {
	RCP<MAT> A_a = Tpetra::MatrixMatrix::add(0.5, false, *A, -0.5, true, *A);
	*fos << "anti-symmetric ";
	calcInfNorm(A_a, false);
}

//  Self explanatory
void calcMaxNonzerosPerRow(const RCP<MAT> &A) {
	size_t result = A->getGlobalMaxNumRowEntries();	
	*fos << "max nonzeros per row:" << result << std::endl;
}

void calcMinNonzerosPerRow(const RCP<MAT> &A) {
	GO rows = A->getGlobalNumRows();
	GO locNonzeros = rows, locMinNonzeros = rows, result = 0;	

	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			locNonzeros = A->getNumEntriesInGlobalRow(row);
			if (locNonzeros >= 0) {
				if (locNonzeros < locMinNonzeros) {
					locMinNonzeros = locNonzeros;
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MIN, 1, &locMinNonzeros, &result);
	*fos << "min nonzeros per row:" << result << std::endl;
}

void calcAvgNonzerosPerRow(const RCP<MAT> &A) {
	GO rows = A->getGlobalNumRows();
	GO locNonzeros = 0, result = 0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			if (A->getNumEntriesInGlobalRow(row) >= 0) {
				locNonzeros += A->getNumEntriesInGlobalRow(row);	
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locNonzeros, &result);
	double castResult = (double)result / (double)rows;
	*fos << "avg nonzeros per row:" << castResult << std::endl;	
}

void calcTrace(const RCP<MAT> &A, bool abs) {
	ST locSum, result = 0.0; 
	typedef Tpetra::Map<LO, GO> map_type; 
	GO numGlobalElements = A->getGlobalNumDiags(); 
	RCP<const map_type> map = rcp(new map_type (numGlobalElements, 0, comm)); 
	VEC v(map);

	A->getLocalDiagCopy(v);
	Teuchos::ArrayRCP<const ST> array = v.getData();	
	for (int i = 0; i < array.size(); i++) {
		if (abs) {
			locSum += array[i];	
		} else {
			locSum += fabs(array[i]);
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locSum, &result);
	if (abs) {
		*fos << "abs trace:" << result << std::endl;
	} else {
		*fos << "trace:" << result << std::endl;
	}
}

void calcAbsTrace(const RCP<MAT> &A) {
	calcTrace(A, true);
}

void calcDummyRows(const RCP<MAT> &A) {
	LO localRows = A->getNodeNumRows(); 
	GO numColsInCurrentRow;
	GO locDummy = 0, result = 0;

	//  Go through each row on the current process
	for (LO row = 0; row < localRows; row++) {
		if (A->getNumEntriesInLocalRow(row) == 1) {
			locDummy++;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locDummy, &result);
	*fos << "dummy rows:" << result << std::endl;
}

//void calcDummyRowsKind(const RCP<MAT> &A) {} 

void calcNumericalSymmetry(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();

	LO localRows = A->getNodeNumRows();
	ArrayView<const ST> valuesA, valuesB;
	ArrayView<const LO> indicesA, indicesB;
	GO numColsInCurrentRow = 0;

	//  Go through each row on the current process
	for (LO row = 0; row < localRows; row++) {
		numColsInCurrentRow = A->getNumEntriesInLocalRow(row); 
		A->getLocalRowView(row, indicesA, valuesA); 
		B->getLocalRowView(row, indicesB, valuesB);
		for (LO col = 0; col < numColsInCurrentRow; col++) {
			if (valuesA[col] != valuesB[col]) {
				*fos << "numerical symmetry:" << 0 << std::endl;
				return;
			}
		} 
	}
	*fos << "numerical symmetry:" << 1 << std::endl;	
}

void calcNonzeroPatternSymmetry(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();

	LO localRows = A->getNodeNumRows();
	ArrayView<const ST> valuesA, valuesB;
	ArrayView<const LO> indicesA, indicesB;
	GO numColsInCurrentRow = 0;

	//  Go through each row on the current process
	for (LO row = 0; row < localRows; row++) {
		numColsInCurrentRow = A->getNumEntriesInLocalRow(row); 
		A->getLocalRowView(row, indicesA, valuesA); 
		B->getLocalRowView(row, indicesB, valuesB);
		//  If the two matrices have the same nonzero indices, symmetric
		for (LO col = 0; col < numColsInCurrentRow; col++) {
			if (indicesA[col] != indicesB[col]) {
				*fos << "nonzero numerical symmetry:" << 0 << std::endl;
				return;
			}
		} 
	}
	*fos << "nonzero numerical symmetry:" << 1 << std::endl;	
}

// 0 not, 1 weak, 2 strict
void calcRowDiagonalDominance(const RCP<MAT> &A) {
	LO localRows = A->getNodeNumRows(); 
	ArrayView<const ST> values;
	ArrayView<const LO> indices;
	GO numColsInCurrentRow;
	ST diagonalEntry, locSum, result = 0.0;

	//  Go through each row on the current process
	for (LO row = 0; row < localRows; row++) {
		diagonalEntry = locSum = 0.0;
		numColsInCurrentRow = A->getNumEntriesInLocalRow(row); 
		A->getLocalRowView(row, indices, values); 
		for (int col = 0; col < numColsInCurrentRow; col++) {
			if (indices[col] == row) {
				locSum += fabs(values[col]);
			} else {
				diagonalEntry = fabs(values[col]);
			}
		}
		if (diagonalEntry > locSum) {
			*fos << "row diag dominance:2" << std::endl;
			return;
		}
		if (diagonalEntry >= locSum) {
			*fos << "row diag dominance:1" << std::endl;
			return;
		}
	}
	*fos << "row diag dominance:0" << std::endl;
}

void calcColDiagonalDominance(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
	calcRowDiagonalDominance(B);
}

void calcDiagonalMean(const RCP<MAT> &A) {
	ST locMean, mean = 0.0; 
	typedef Tpetra::Map<LO, GO> map_type; 
	GO numGlobalElements = A->getGlobalNumDiags(); 
	RCP<const map_type> map = rcp(new map_type (numGlobalElements, 0, comm)); 
	VEC v(map);
	A->getLocalDiagCopy(v);
	Teuchos::ArrayRCP<const ST> array = v.getData();	
	for (int i = 0; i < array.size(); i++) {
		locMean += array[i];	
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locMean, &mean);
	mean /= A->getGlobalNumRows();
	*fos << "diag mean" << mean << ", " << std::endl;
}

// indicates the diagonal sign pattern
// -2 all negative, -1 nonpositive, 0 all zero, 1 nonnegative, 2 all positive, 
// 3 some negative,some or no zero,some positive
void calcDiagonalSign(const RCP<MAT> &A) {
	long locPos = 0, locNeg = 0, locZero = 0;
	long totalPos, totalNeg, totalZero;
	typedef Tpetra::Map<LO, GO> map_type;
	GO numGlobalElements = A->getGlobalNumDiags();
	RCP<const map_type> map = rcp(new map_type (numGlobalElements, 0, comm));
	VEC v(map);
	A->getLocalDiagCopy(v);
	Teuchos::ArrayRCP<const ST> array = v.getData();
	for (int i = 0; i < array.size(); i++) {
		if (array[i] > 0) {
			locPos++;
		} else if (array[i] < 0) {
			locNeg++;
		} else {
			locZero++;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locPos, &totalPos);
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locNeg, &totalNeg);
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locZero, &totalZero);
	*fos << "diagonal sign:";
	if (totalPos > 0 && totalNeg == 0 && totalZero == 0) {
		*fos << "2" << std::endl;
	} else if (totalNeg > 0 && totalPos == 0 && totalZero == 0) {
		*fos << "-2" << std::endl;
	} else if (totalZero > 0 && totalPos == 0 && totalNeg == 0) {
		*fos << "0" << std::endl;
	} else if (totalNeg == 0) {
		*fos << "1" << std::endl;
	} else if (totalPos == 0) {
		*fos << "-1" << std::endl;
	} else {
		*fos << "3" << std::endl;
	}
}

void calcDiagonalNonzeros(const RCP<MAT> &A) {
	GO diagNonzeros = A->getGlobalNumDiags();
	*fos << "nonzeros on diag:" << diagNonzeros << std::endl;
}

void calcLowerBandwidth(const RCP<MAT> &A) {

	// Maybe doing something with a map
	// GO gblRow =  map->getGlobalElement(lclRow);
	// From the power method example code
	ArrayView<const ST> values;
	ArrayView<const LO> indices;
	GO numColsInCurrentRow;
	ST mean, locVariance, locMaxVariance, result = 0.0;
	std::cout << comm->getRank() << "\t" << A->getNodeNumRows() << "\t" << 
	A->getNodeNumCols() << std::endl;
}

void calcUpperBandwidth(const RCP<MAT> &A) {
	int localRows = A->getNodeNumRows();
	int* array = new int[numNodes];
	GO numColsInCurrentRow, result = 0, ub = 0, maxUB = 0;
	ArrayView<const ST> values;
	ArrayView<const LO> indices;

	Teuchos::gatherAll<int, int>(*comm, 1, &localRows, numNodes, array);
	int startingRow = 0; 
	for (int i = 0; i < comm->getRank(); i++) {
		startingRow += array[i];
	}
	for (int row = 0; row < localRows; row++) {
		A->getLocalRowView(row, indices, values);
		ub = indices.back() - (row + startingRow);
		if (ub > maxUB) {
			maxUB = ub;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &maxUB, &maxUB);
	*fos << "ub:" << maxUB << std::endl;
}
