#include "tpetra_properties_crsmatrix.h"

//  Dimension of the square matrix
size_t calcDim(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeDim);
	//*fos << "dimension:" << A->getGlobalNumRows() << ", " << std::endl;
	return A->getGlobalNumRows();
}

//  Frobenius norm of matrix
ST calcFrobeniusNorm(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeFrobeniusNorm);
	//*fos << "frob norm:" << A->getFrobeniusNorm() << ", " << std::endl;
	return A->getFrobeniusNorm();
}

//  Symmetric A_s = (A+A')/2
ST calcSymmetricFrobeniusNorm(const RCP<MAT> &A){ 
	TimeMonitor LocalTimer (*timeSymmetricFrobeniusNorm);
	RCP<MAT> A_s = Tpetra::MatrixMatrix::add(0.5, false, *A, 0.5, true, *A);
	//*fos << "symm frob norm:" << A_s->getFrobeniusNorm() << ", " << std::endl;
	return A_s->getFrobeniusNorm();
}

//  Antisymmetric A_a = (A-A')/2
ST calcAntisymmetricFrobeniusNorm(const RCP<MAT> &A){ 
	TimeMonitor LocalTimer (*timeAntisymmetricFrobeniusNorm);
	RCP<MAT> A_a = Tpetra::MatrixMatrix::add(0.5, false, *A, -0.5, true, *A);
	//*fos << "antisymm frob norm:" << A_a->getFrobeniusNorm() << ", " << std::endl;
	return A_a->getFrobeniusNorm();
}

//  Self explanatory
size_t calcMaxNonzerosPerRow(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeMaxNonzerosPerRow);
	return A->getGlobalMaxNumRowEntries();
}

size_t calcDiagonalNonzeros(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeDiagonalNonzeros);
	return A->getGlobalNumDiags();
}