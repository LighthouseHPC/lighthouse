#include "tpetra_properties_crsmatrix.h"

//  Transpose the matrix, get row locVariance 
ST calcColVariance(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeColVariance);
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
	
	GO rows = B->getGlobalNumRows(); 
	ST mean, locVariance, locMaxVariance, result = 0.0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		comm->barrier();
		if (B->getRowMap()->isNodeGlobalElement(row)) {
			mean = locVariance = 0.0; 
			size_t cols = B->getNumEntriesInGlobalRow(row); 
			Array<ST> values(cols);
			Array<GO> indices(cols);
			B->getGlobalRowCopy(row, indices(), values(), cols);
		//  Two-step approach for locVariance, could be more efficient 
			for (LO col = 0; col < cols; col++) {
				mean += values[col];
			} 
		//  Divide entries by the dim (to include zeros)
			mean /= B->getGlobalNumCols();
			for (LO col = 0; col < cols; col++) {
				locVariance += (values[col] - mean) * (values[col] - mean);
			}
			locVariance /= B->getGlobalNumCols();
			if (locVariance > locMaxVariance) locMaxVariance = locVariance;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxVariance, &result);
	return result;
	//*fos << "col variance:" << result << std::endl;
}