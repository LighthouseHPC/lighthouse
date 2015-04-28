#include "tpetra_properties_crsmatrix.h"

//  Max absolute row sum
ST calcInfNorm(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeInfNorm);
	GO rows = A->getGlobalNumRows(); 
	ST locSum, locMaxSum, result = 0.0;
	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			locSum = 0;
			size_t cols = A->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols); 
			for (LO col = 0; col < cols; col++) {
				locSum += fabs(values[col]);
			} 
			if (locSum > locMaxSum) {
				locMaxSum = locSum;
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxSum, &result);
	//*fos << "inf norm:" << result << std::endl;
	return result;
}

//  Max absolute row sum of symmetric part
ST calcSymmetricInfNorm(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeSymmetricInfNorm);
	RCP<MAT> A_s = Tpetra::MatrixMatrix::add(0.5, false, *A, 0.5, true, *A);
	//*fos << "symmetric ";
	return calcInfNorm(A_s);
}

//  Max absolute row sum of anti-symmetric part
ST calcAntisymmetricInfNorm(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeAntisymmetricInfNorm);
	RCP<MAT> A_a = Tpetra::MatrixMatrix::add(0.5, false, *A, -0.5, true, *A);
	//*fos << "anti-symmetric ";
	return calcInfNorm(A_a);
}
