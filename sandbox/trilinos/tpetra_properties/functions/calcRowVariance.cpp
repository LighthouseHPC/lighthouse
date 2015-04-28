#include "tpetra_properties_crsmatrix.h"

//  Return the maximum row locVariance for the matrix
//  The average of the squared differences from the Mean.
ST calcRowVariance(const RCP<MAT> &A) {
	TimeMonitor LocalTimer (*timeRowVariance);
	GO rows = A->getGlobalNumRows(); 
	ST mean, locVariance, locMaxVariance, result = 0.0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		comm->barrier();
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			mean = locVariance = 0.0; 
			size_t cols = A->getNumEntriesInGlobalRow(row); 
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols); 
		//  Two-step approach for locVariance, could be more efficient 
			for (LO col = 0; col < cols; col++) {
				mean += values[col];
			} 
		//  Divide entries by the dim (to include zeros)
			mean /= A->getGlobalNumCols();
			for (LO col = 0; col < cols; col++) {
				locVariance += (values[col] - mean) * (values[col] - mean);
			}
			locVariance /= cols;
			std::cout << "lmv" << myRank << "\t" << row << "\t" << locVariance << std::endl;
			if (locVariance > locMaxVariance) {
				locMaxVariance = locVariance;
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxVariance, &result);
	return result;
	//*fos << "row variance:" << result << std::endl;
}
ST calcRowVariance(const RCP<MATC> &A) {
	TimeMonitor LocalTimer (*timeRowVariance);
	GO rows = A->getGlobalNumRows(); 
	ST mean, locVariance, locMaxVariance, result = 0.0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		comm->barrier();
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			mean = locVariance = 0.0; 
			size_t cols = A->getNumEntriesInGlobalRow(row); 
			Array<STC> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols); 
		//  Two-step approach for locVariance, could be more efficient 
			for (LO col = 0; col < cols; col++) {
				mean += std::real(values[col]);
			} 
		//  Divide entries by the dim (to include zeros)
			mean /= A->getGlobalNumCols();
			for (LO col = 0; col < cols; col++) {
				locVariance += (std::real(values[col]) - mean) * (std::real(values[col]) - mean);
			}
			locVariance /= A->getGlobalNumCols();
			std::cout << "lmv" << myRank << "\t" << row << "\t" << locVariance << std::endl;
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