#include "tpetra_properties.h"
int main(int argc, char *argv[]) {
	
	//  General setup for Teuchos/communication
	Teuchos::GlobalMPISession mpiSession(&argc, &argv);
	Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
	Teuchos::RCP<const Teuchos::Comm<int> > comm = platform.getComm();
	Teuchos::RCP<Node> node = platform.getNode();
	int myRank = comm->getRank();
	Teuchos::oblackholestream blackhole;
	std::ostream& out = (myRank == 0) ? std::cout : blackhole;
	RCP<Teuchos::FancyOStream> fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));

	// Load and run tests on Matrix Market file
	std::string filename("../demo.mtx");
	RCP<MAT> A = Tpetra::MatrixMarket::Reader<MAT>::readSparseFile(filename, comm, node);
	runGauntlet(A);

}
void runGauntlet(RCP<MAT> &A) {
	// Test squareness
	if (A->getGlobalNumRows() != A->getGlobalNumCols() ) {
		std::cout << "Not a square matrix, exiting\n";
		exit(-1);
	}
	calcRowVariance(A);
	calcDim(A);

}

//  Return the maximum row variance for the matrix
//  The average of the squared differences from the Mean.
//  We cannot get a globalRowView due to the matrix being locally indexed
void calcRowVariance(RCP<MAT> &A) {
	int dim = A->getGlobalNumCols();
	Teuchos::Array<int> indices(dim);
	Teuchos::Array<double> values(dim);
	std::size_t numEntries = 0;
	double mean, sum, variance, maxVariance = 0.0;

	for (int row = 0; row < dim; row++) {
		mean = 0.0; variance = 0.0;
		A->getGlobalRowCopy(row, indices, values, numEntries);
		for (int col = 0; col < numEntries; col++) {
			mean += values[col];
		} 
		mean /= dim;
		for (int col = 0; col < numEntries; col++) {
			variance += (values[col] - mean) * (values[col] - mean);
		}
		variance /= dim;
		if (variance > maxVariance) maxVariance = variance;
	}
	std::cout << maxVariance << std::endl;
}

void calcColVariance(RCP<MAT> &A) {

}

void calcDiagVariance(RCP<MAT> &A) {

}

void calcNonzeros(RCP<MAT> &A) {

}

void calcDim(RCP<MAT> &A) {
	std::cout << A->getGlobalNumRows() << ", " << std::endl;
}

//  Already implemented in Tpetra
void calcFrobeniusNorm(RCP<MAT> &A) {
	std::cout << A->getFrobeniusNorm() << ", " << std::endl;
}
void calcSymmetricFrobeniusNorm(RCP<MAT> &A);
void calcAntisymmetricFrobeniusNorm(RCP<MAT> &A);
void calcOneNorm(RCP<MAT> &A);
void calcInfNorm(RCP<MAT> &A);
void calcSymmetricInfNorm(RCP<MAT> &A);
void calcAntisymmetricInfNorm(RCP<MAT> &A);
void calcMaxNonzerosPerRow(RCP<MAT> &A);
void calcTrace(RCP<MAT> &A);
void calcAbsTrace(RCP<MAT> &A);
void calcMinNonzerosPerRow(RCP<MAT> &A);
void calcAvgNonzerosPerRow(RCP<MAT> &A);
void calcDummyRows(RCP<MAT> &A);
void calcDummyRowsKind(RCP<MAT> &A);
void calcNumericalSymmetry(RCP<MAT> &A);
void calcNonzeroPatternSymmetry(RCP<MAT> &A);
void calcNumericalValueSymmetry(RCP<MAT> &A);
void calcNonzeroPatternSymmetry(RCP<MAT> &A);
void calcRowDiagonalDominance(RCP<MAT> &A);
void calcColDiagonalDominance(RCP<MAT> &A);
void calcDiagonalAvg(RCP<MAT> &A);
void calcDiagonalSign(RCP<MAT> &A);
void calcDiagonalNonzeros(RCP<MAT> &A);
void calcLowerBandwidth(RCP<MAT> &A);
void calcUpperBandwidth(RCP<MAT> &A);


