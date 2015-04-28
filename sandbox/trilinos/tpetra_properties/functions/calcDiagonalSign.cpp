#include "tpetra_properties_crsmatrix.h"

// indicates the diagonal sign pattern
// -2 all negative, -1 nonpositive, 0 all zero, 1 nonnegative, 2 all positive, 
// 3 some negative,some or no zero,some positive
int calcDiagonalSign(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeDiagonalSign);
	long locPos = 0, locNeg = 0, locZero = 0;
	long totalPos, totalNeg, totalZero;
	GO rows = A->getGlobalNumRows();
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols);
			for (size_t col = 0; col < cols; col++) {
				if (indices[col] == row) {
					if (values[col] > 0) {
						locPos++;
					} else if (values[col] < 0) {
						locNeg++;
					} else {
						locZero++;
					}
				}
			}
		}
	}

	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locPos, &totalPos);
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locNeg, &totalNeg);
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locZero, &totalZero);
	//*fos << "diagonal sign:";
	if (totalPos > 0 && totalNeg == 0 && totalZero == 0) {
		//*fos << "2" << std::endl;
		return 2;
	} else if (totalNeg > 0 && totalPos == 0 && totalZero == 0) {
		//*fos << "-2" << std::endl;
		return -2;
	} else if (totalZero > 0 && totalPos == 0 && totalNeg == 0) {
		//*fos << "0" << std::endl;
		return 0;
	} else if (totalNeg == 0) {
		//*fos << "1" << std::endl;
		return 1;
	} else if (totalPos == 0) {
		//*fos << "-1" << std::endl;
		return -1;
	} else {
		//*fos << "3" << std::endl;
		return 3;
	}
}