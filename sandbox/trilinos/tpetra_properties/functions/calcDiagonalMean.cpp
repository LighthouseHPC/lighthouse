#include "tpetra_properties_crsmatrix.h"

ST calcDiagonalMean(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeDiagonalMean);
	ST locMean, mean = 0.0;
  GO rows = A->getGlobalNumRows();
  for (GO row = 0; row < rows; row++) {
    if (A->getRowMap()->isNodeGlobalElement(row)) {
      size_t cols = A->getNumEntriesInGlobalRow(row);
      Array<ST> values(cols);
      Array<GO> indices(cols);
      A->getGlobalRowCopy(row, indices(), values(), cols);
      for (size_t col = 0; col < cols; col++) {
        if (indices[col] == row) {
          locMean += values[col]; 
        }
      }
    }
  }
  Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locMean, &mean);
  mean /= A->getGlobalNumRows();
  //*fos << "diag mean" << mean << ", " << std::endl;
  return mean;
}