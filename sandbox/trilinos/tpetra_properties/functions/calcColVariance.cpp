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
			for (LO col = cols; col < B->getGlobalNumCols(); col++) {
				locVariance += (-mean) * (-mean);
			}
			locVariance /= B->getGlobalNumCols();
			if (locVariance > locMaxVariance) locMaxVariance = locVariance;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxVariance, &result);
	return result;
	//*fos << "col variance:" << result << std::endl;
}

ST calcColVariance(const RCP<MATC> &A) {
	TimeMonitor LocalTimer (*timeColVariance);
	Tpetra::RowMatrixTransposer<STC, LO, GO, NT> transposer(A);
	RCP<MATC> B = transposer.createTranspose();

	GO rows = B->getGlobalNumRows(); 
	ST mean, locVariance, locMaxVariance, result = 0.0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		comm->barrier();
		if (B->getRowMap()->isNodeGlobalElement(row)) {
			mean = locVariance = 0.0; 
			size_t cols = B->getNumEntriesInGlobalRow(row); 
			Array<STC> values(cols);
			Array<GO> indices(cols);
			B->getGlobalRowCopy(row, indices(), values(), cols); 
		//  Two-step approach for locVariance, could be more efficient 
			for (LO col = 0; col < cols; col++) {
				mean += std::real(values[col]);
			} 
		//  Divide entries by the dim (to include zeros)
			mean /= B->getGlobalNumCols();
			for (LO col = 0; col < cols; col++) {
				locVariance += (std::real(values[col]) - mean) * (std::real(values[col]) - mean);
			}
			for (LO col = cols; col < B->getGlobalNumCols(); col++) {
				locVariance += (-mean) * (-mean);
			}
			locVariance /= B->getGlobalNumCols();
			if (locVariance > locMaxVariance) {
				locMaxVariance = locVariance;
			}
		}
	}
	double l = std::abs(locMaxVariance);
	double r;
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &l, &r);
	return r;
	//*fos << "row variance:" << result << std::endl;
}
