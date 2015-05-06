#include "tpetra_properties_crsmatrix.h"

size_t calcMinNonzerosPerRow(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeMinNonzerosPerRow);
	size_t rows = A->getGlobalNumRows();
	size_t locNonzeros = rows, locMinNonzeros = rows, result = 0;	

	for (size_t row = 0; row < rows; row++) {
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
	//*fos << "min nonzeros per row:" << result << std::endl;
	return result;
}

ST calcAvgNonzerosPerRow(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeAvgNonzerosPerRow);
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
	return (ST)result / (ST)rows;
}
