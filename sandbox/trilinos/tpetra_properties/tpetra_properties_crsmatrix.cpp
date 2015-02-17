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
	std::string filename("../demo.mtx");
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
	calcRowVariance(A, false);
	calcColVariance(A);
	calcDiagVariance(A);
	calcNonzeros(A);
	calcDim(A);
	calcFrobeniusNorm(A);
	calcSymmetricFrobeniusNorm(A);
	calcAntisymmetricFrobeniusNorm(A);
	calcInfNorm(A, false);
	calcOneNorm(A);
	calcSymmetricInfNorm(A);
	calcAntisymmetricInfNorm(A);
	calcMaxNonzerosPerRow(A);
}

//  Return the maximum row locVariance for the matrix
//  The average of the squared differences from the Mean.
void calcRowVariance(const RCP<MAT> &A, bool transpose) {
	LO localRows = A->getNodeNumRows(); 
	ArrayView<const ST> values;
	ArrayView<const LO> indices;
	GO numColsInCurrentRow;
	ST mean, locVariance, locMaxVariance, result = 0.0;

	//  Go through each row on the current process
	for (LO row = 0; row < localRows; row++) {
		mean = locVariance = 0.0; 
		numColsInCurrentRow = A->getNumEntriesInLocalRow(row); 
		A->getLocalRowView(row, indices, values); 

		//  Two-step approach for locVariance, could be more efficient 
		for (LO col = 0; col < numColsInCurrentRow; col++) {
			mean += values[col];
		} 
		//  Divide entries by the dim (to include zeros)
		mean /= A->getGlobalNumCols();
		for (LO col = 0; col < numColsInCurrentRow; col++) {
			locVariance += (values[col] - mean) * (values[col] - mean);
		}
		locVariance /= A->getGlobalNumCols();
		if (locVariance > locMaxVariance) locMaxVariance = locVariance;
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxVariance, &result);
	if (transpose) {
		*fos << "col variance:" << result << std::endl;
	} else {
		*fos << "row variance:" << result << std::endl;
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
		numColsInCurrentRow = A->getNumEntriesInLocalRow(row); 
		A->getLocalRowView(row, indices, values); 

		for (LO col = 0; col < numColsInCurrentRow; col++) {
			locSum += fabs(values[col]);
		} 
		if (locSum > locMaxSum) locMaxSum = locSum;
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

void calcSymmetricInfNorm(const RCP<MAT> &A) {

}
void calcAntisymmetricInfNorm(const RCP<MAT> &A) {

}
void calcMaxNonzerosPerRow(const RCP<MAT> &A) {
	
}

void calcDiagMean(const RCP<MAT> &A) {
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

