#include "tpetra_properties_crsmatrix.h"

size_t calcLowerBandwidth(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeLowerBandwidth);
	size_t rows = A->getGlobalNumRows();
	size_t localMaxLB = 0, localLB = 0, totalLB;
	size_t minIndex;

	for (size_t row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			if (cols > 0 && cols <= A->getGlobalNumRows()) {
				Array<ST> values(cols);
				Array<GO> indices(cols);
				A->getGlobalRowCopy(row, indices(), values(), cols); 
				minIndex = indices[0];
				for (size_t col = 1; col < cols; col++) {
					if (indices[col] < minIndex) {
						minIndex = indices[col];
					}	
				}
				localLB = row - minIndex;
				if (row < minIndex) {
					localLB = 0;
				}
				if (localLB > localMaxLB) {
					localMaxLB = localLB;
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &localMaxLB, &totalLB);
	//*fos << "lb:" << totalLB << std::endl;
	return totalLB;
}

size_t calcUpperBandwidth(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeUpperBandwidth);
	size_t rows = A->getGlobalNumRows();
	size_t localMaxUB = 0, localUB = 0, totalUB;
	size_t maxIndex;

	for (size_t row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			if (cols > 0 && cols <= A->getGlobalNumRows()) {
				Array<ST> values(cols);
				Array<GO> indices(cols);
				A->getGlobalRowCopy(row, indices(), values(), cols); 
				maxIndex = indices[0];
				for (size_t col = 1; col < cols; col++) {
					if (indices[col] > maxIndex) {
						maxIndex = indices[col];
					}	
				}
				localUB = maxIndex - row;
				if (row > maxIndex) {
					localUB = 0;
				}
				if (localUB > localMaxUB) {
					localMaxUB = localUB;
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &localMaxUB, &totalUB);
	//*fos << "ub:" << totalUB << std::endl;
	return totalUB;
}