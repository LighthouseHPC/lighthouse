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
	RCP<MAT> A = Tpetra::MatrixMarket::Reader<MAT>::readSparseFile(filename, comm, node);
	runGauntlet(A);

}
void runGauntlet(RCP<MAT> &A) {
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
}

//  Return the maximum row locVariance for the matrix
//  The average of the squared differences from the Mean.
void calcRowVariance(RCP<MAT> &A, bool transpose) {
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
void calcColVariance(RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
	calcRowVariance(B, true);
}

//  The variance of the diagonal
void calcDiagVariance(RCP<MAT> &A) {
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
void calcNonzeros(RCP<MAT> &A) {
	*fos << "nonzeros:" << A->getGlobalNumEntries() << ", " << std::endl;
}

//  Dimension of the square matrix
void calcDim(RCP<MAT> &A) {
	*fos << "dimension:" << A->getGlobalNumRows() << ", " << std::endl;
}

//  Frobenius norm of matrix
void calcFrobeniusNorm(RCP<MAT> &A) {
	*fos << "frob norm:" << A->getFrobeniusNorm() << ", " << std::endl;
}

//  Symmetric A_s = (A+A')/2
void calcSymmetricFrobeniusNorm(RCP<MAT> &A){ 
	RCP<MAT> A_s;
	Tpetra::MatrixMatrix::Add(*A, false, 0.5, *A, true, 0.5, A_s);
	*fos << "symm frob norm" << A_s->getFrobeniusNorm() << ", " << std::endl;
}

//  Antisymmetric A_a = (A-A')/2
void calcAntisymmetricFrobeniusNorm(RCP<MAT> &A){ 

}
void calcOneNorm(RCP<MAT> &A){ }
void calcInfNorm(RCP<MAT> &A){ }

void calcDiagAvg(RCP<MAT> &A) {
	ST mean, locVariance, locMaxVariance = 0.0; //normal things
	typedef Tpetra::Map<LO, GO> map_type; //basic map setup
	const GO indexBase = 0; //idk
	GO numGlobalElements = A->getGlobalNumDiags(); //just need the space for diagonal entries
	RCP<const map_type> map = rcp(new map_type (numGlobalElements, indexBase, comm)); //map itself
	VEC v(map);
	A->getLocalDiagCopy(v);
	*fos << "avg:" << v.meanValue() << std::endl;
}

