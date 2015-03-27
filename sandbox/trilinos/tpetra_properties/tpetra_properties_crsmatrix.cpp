#include "tpetra_properties_crsmatrix.h"

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int numNodes;
int myRank;

int main(int argc, char *argv[]) {
	
	//  General setup for Teuchos/communication
	Teuchos::GlobalMPISession mpiSession(&argc, &argv);
	Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
  comm = platform.getComm();
  RCP<NT> node = platform.getNode();
  myRank = comm->getRank(); 

  Teuchos::oblackholestream blackhole;
  std::ostream& out = (myRank == 0) ? std::cout : blackhole;
  fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));

	// Load and run tests on Matrix Market file
	if (argc < 2) {
		*fos << "Error: no file was selected" << std::endl;
		exit(-2);
	}	
  std::string filename(argv[1]);
  RCP<MAT> A = Reader::readSparseFile(filename, comm, node, true);
  Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
  runGauntlet(A);
  calcSmallestEigenvalues(A, filename);
}

void runGauntlet(const RCP<MAT> &A) {
	// Test squareness
	if (A->getGlobalNumRows() != A->getGlobalNumCols() ) {
		*fos << "Not a square matrix, exiting." << std::endl;
		exit(-1);
	}
	*fos << comm->getSize() << ", ";
	*fos << calcRowVariance(A) << ", ";
	*fos << calcColVariance(A) << ", ";
	*fos << calcDiagVariance(A) << ", ";
	*fos << calcNonzeros(A) << ", ";
	*fos << calcDim(A) << ", ";
	*fos << calcFrobeniusNorm(A) << ", ";
	*fos << calcSymmetricFrobeniusNorm(A) << ", ";
	*fos << calcAntisymmetricFrobeniusNorm(A) << ", ";
	*fos << calcOneNorm(A) << ", ";
	*fos << calcInfNorm(A) << ", ";
	*fos << calcSymmetricInfNorm(A) << ", ";
	*fos << calcAntisymmetricInfNorm(A) << ", ";
	*fos << calcMaxNonzerosPerRow(A) << ", ";
	*fos << calcMinNonzerosPerRow(A) << ", ";
	*fos << calcAvgNonzerosPerRow(A) << ", ";
	*fos << calcTrace(A) << ", ";
	*fos << calcAbsTrace(A) << ", ";
	*fos << calcDummyRows(A) << ", ";
	calcSymmetry(A);
	*fos << calcRowDiagonalDominance(A) << ", ";
	*fos << calcColDiagonalDominance(A) << ", ";
	*fos << calcLowerBandwidth(A) << ", ";
	*fos << calcUpperBandwidth(A) << ", ";
	*fos << calcDiagonalMean(A) << ", ";
	*fos << calcDiagonalSign(A) << ", ";
	*fos << calcDiagonalNonzeros(A) << ", ";
  calcEigenValues(A, "LM");
  calcEigenValues(A, "SM");
  calcEigenValues(A, "LR");
  calcEigenValues(A, "SR"); 
  *fos << std::endl;
  
}

//  Return the maximum row locVariance for the matrix
//  The average of the squared differences from the Mean.
ST calcRowVariance(const RCP<MAT> &A) {
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
			locVariance /= A->getGlobalNumCols();
			if (locVariance > locMaxVariance) locMaxVariance = locVariance;
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxVariance, &result);
	return result;
	//*fos << "row variance:" << result << std::endl;
}

//  Transpose the matrix, get row locVariance 
ST calcColVariance(const RCP<MAT> &A) {
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

//  The variance of the diagonal
ST calcDiagVariance(const RCP<MAT> &A) {
	GO rows = A->getGlobalNumRows(); 
	ST locMean = 0.0; 
	ST mean = 0.0, locVariance = 0.0, result = 0.0;

	//  Go through each row on the current process
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
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols);
			for (size_t col = 0; col < cols; col++) {
				if (indices[col] == row) {
					locVariance += (values[col] - mean) * (values[col] - mean);
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locVariance, &result);
	result /= A->getGlobalNumRows();
	return result;
	//*fos << "diag variance:" << result << std::endl;
}

//  Total number of nonzeros in matrix
size_t calcNonzeros(const RCP<MAT> &A) {
	//*fos << "nonzeros:" << A->getGlobalNumEntries() << ", " << std::endl;
	return A->getGlobalNumEntries();
}

//  Dimension of the square matrix
size_t calcDim(const RCP<MAT> &A) {
	//*fos << "dimension:" << A->getGlobalNumRows() << ", " << std::endl;
	return A->getGlobalNumRows();
}

//  Frobenius norm of matrix
ST calcFrobeniusNorm(const RCP<MAT> &A) {
	//*fos << "frob norm:" << A->getFrobeniusNorm() << ", " << std::endl;
	return A->getFrobeniusNorm();
}

//  Symmetric A_s = (A+A')/2
ST calcSymmetricFrobeniusNorm(const RCP<MAT> &A){ 
	RCP<MAT> A_s = Tpetra::MatrixMatrix::add(0.5, false, *A, 0.5, true, *A);
	//*fos << "symm frob norm:" << A_s->getFrobeniusNorm() << ", " << std::endl;
	return A_s->getFrobeniusNorm();
}

//  Antisymmetric A_a = (A-A')/2
ST calcAntisymmetricFrobeniusNorm(const RCP<MAT> &A){ 
	RCP<MAT> A_a = Tpetra::MatrixMatrix::add(0.5, false, *A, -0.5, true, *A);
	//*fos << "antisymm frob norm:" << A_a->getFrobeniusNorm() << ", " << std::endl;
	return A_a->getFrobeniusNorm();
}

//  Max absolute row sum
ST calcInfNorm(const RCP<MAT> &A) {
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

//  Max absolute column sum
ST calcOneNorm(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();

	GO rows = B->getGlobalNumRows(); 
	ST locSum, locMaxSum, result = 0.0;
	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		if (B->getRowMap()->isNodeGlobalElement(row)) {
			locSum = 0;
			size_t cols = B->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			B->getGlobalRowCopy(row, indices(), values(), cols); 
			for (LO col = 0; col < cols; col++) {
				locSum += fabs(values[col]);
			} 
			if (locSum > locMaxSum) {
				locMaxSum = locSum;
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &locMaxSum, &result);
	//*fos << "one norm:" << result << std::endl;
	return result;
}

//  Max absolute row sum of symmetric part
ST calcSymmetricInfNorm(const RCP<MAT> &A) {
	RCP<MAT> A_s = Tpetra::MatrixMatrix::add(0.5, false, *A, 0.5, true, *A);
	//*fos << "symmetric ";
	return calcInfNorm(A_s);
}

//  Max absolute row sum of anti-symmetric part
ST calcAntisymmetricInfNorm(const RCP<MAT> &A) {
	RCP<MAT> A_a = Tpetra::MatrixMatrix::add(0.5, false, *A, -0.5, true, *A);
	//*fos << "anti-symmetric ";
	return calcInfNorm(A_a);
}

//  Self explanatory
size_t calcMaxNonzerosPerRow(const RCP<MAT> &A) {
	return A->getGlobalMaxNumRowEntries();
}

size_t calcMinNonzerosPerRow(const RCP<MAT> &A) {
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

ST calcTrace(const RCP<MAT> &A) {
	GO rows = A->getGlobalNumRows(); 
	ST trace = 0.0, result = 0.0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols);
			for (size_t col = 0; col < cols; col++) {
				if (indices[col] == row) {
          trace += values[col]; 
        }
      }
    }
  }
  Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &trace, &result);
  //*fos << "trace:" << result << std::endl;
  return result;
}

ST calcAbsTrace(const RCP<MAT> &A) {
	GO rows = A->getGlobalNumRows(); 
	ST trace = 0.0, result = 0.0;

	//  Go through each row on the current process
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols);
			for (size_t col = 0; col < cols; col++) {
				if (indices[col] == row) {
					trace += fabs(values[col]);
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &trace, &result);
	//*fos << "trace:" << result << std::endl;	
	return result;
}

size_t calcDummyRows(const RCP<MAT> &A) {
	size_t rows = A->getGlobalNumRows(); 
	size_t locDummy = 0, result = 0;

	//  Go through each row on the current process
	for (size_t row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			if (A->getNumEntriesInGlobalRow(row) == 1) {
				locDummy++;
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &locDummy, &result);
	//*fos << "dummy rows:" << result << std::endl;
	return result;
}

std::vector<ST> calcSymmetry(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();

	GO rows = A->getGlobalNumRows(); 
	ST result = 0.0;
	GO match = 0, noMatch = 0, dne = 0;
	GO totalMatch, totalNoMatch, totalDne;
	GO locEntries = 0;

	GO diagNonzeros = A->getGlobalNumDiags();
	GO offDiagNonzeros = A->getGlobalNumEntries() - diagNonzeros;
	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t colsA = A->getNumEntriesInGlobalRow(row);
			size_t colsB = B->getNumEntriesInGlobalRow(row);
			Array<ST> valuesA(colsA), valuesB(colsB);
			Array<GO> indicesA(colsA), indicesB(colsB);
			A->getGlobalRowCopy(row, indicesA(), valuesA(), colsA); 
			B->getGlobalRowCopy(row, indicesB(), valuesB(), colsB);

			//  Make maps for each row, ignoring diagonal
			std::map<GO, ST> mapA, mapB;
			for (int colA = 0; colA < colsA; colA++) {
				if (row != indicesA[colA])
					mapA.insert( std::pair<GO,ST>(indicesA[colA], valuesA[colA]) );
			}
			for (int colB = 0; colB < colsB; colB++) {
				if (row != indicesB[colB])
					mapB.insert( std::pair<GO,ST>(indicesB[colB], valuesB[colB]) );
			}
			//  Compare the maps
			std::map<GO, ST>::iterator iterA;
			for (iterA = mapA.begin(); iterA != mapA.end(); iterA++) {
				//*fos << row << ": A[" << iterA->first << "]:" << iterA->second << std::endl;
				//*fos << row << ": B[" << iterA->first << "]:" << mapB[iterA->first] << std::endl;
				//  Matching indices found
				if (mapB.count (iterA->first) ) {
					//  Check if values for those indices match
					if ( iterA->second == mapB[iterA->first] ) {
						match++;
					} else {
						noMatch++;
					}
				} else {
					dne++;
				}
			}
		}
	}
	std::vector<ST> results;
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &match, &totalMatch);
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &noMatch, &totalNoMatch);
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_SUM, 1, &dne, &totalDne);
	results.push_back( (double)totalMatch/(double)offDiagNonzeros );
	results.push_back( (double)totalNoMatch/(double)offDiagNonzeros );
	results.push_back( (double)totalDne/(double)offDiagNonzeros );
	*fos << (double)totalMatch/(double)offDiagNonzeros << ", ";
	*fos << (double)totalNoMatch/(double)offDiagNonzeros << ", ";
	*fos << (double)totalDne/(double)offDiagNonzeros << ", "; 
	return results;
}

// 0 not, 1 weak, 2 strict
// a_ii >= sum(a_ij) for all i,js i!=j
int calcRowDiagonalDominance(const RCP<MAT> &A) {
	GO rows = A->getGlobalNumRows(); 
	ST result = 0.0;
	GO totalMatch, match = 0;
	GO locEntries = 0;
	ST locDiagSum = 0.0, locRowSum = 0.0;
	ST totalDiagSum, totalRowSum;
	int strict = 1, totalStrict;

	for (GO row = 0; row < rows; row++) {
		if (A->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = A->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			A->getGlobalRowCopy(row, indices(), values(), cols); 
			if (cols < A->getGlobalNumRows()) {
				totalDiagSum = totalRowSum = 0.0;
				for (size_t col = 0; col < cols; col++) {
					if (row == indices[col]) {
						totalDiagSum += values[col];
					} else {
						totalRowSum += values[col];
					}
				}
				if (locDiagSum < locRowSum) {
					//*fos << "row diagonal dominance:0" << std::endl;
					return 0;
				} else if (locDiagSum == locRowSum) {
					strict = 0;
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &strict, &totalStrict);
	if (totalStrict == 1) {
		//*fos << "row diagonal dominance:2" << std::endl;
		return 2;
	} else {
		//*fos << "row diagonal dominance:1" << std::endl;
		return 1;
	}
}

int calcColDiagonalDominance(const RCP<MAT> &A) {
	Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();

	GO rows = B->getGlobalNumRows(); 
	ST result = 0.0;
	GO totalMatch, match = 0;
	GO locEntries = 0;
	ST locDiagSum = 0.0, locRowSum = 0.0;
	ST totalDiagSum, totalRowSum;
	int strict = 1, totalStrict;

	for (GO row = 0; row < rows; row++) {
		if (B->getRowMap()->isNodeGlobalElement(row)) {
			size_t cols = B->getNumEntriesInGlobalRow(row);
			Array<ST> values(cols);
			Array<GO> indices(cols);
			B->getGlobalRowCopy(row, indices(), values(), cols); 
			if (cols < B->getGlobalNumRows()) {
				totalDiagSum = totalRowSum = 0.0;
				for (size_t col = 0; col < cols; col++) {
					if (row == indices[col]) {
						totalDiagSum += values[col];
					} else {
						totalRowSum += values[col];
					}
				}
				if (locDiagSum < locRowSum) {
					//*fos << "col diagonal dominance:0" << std::endl;
					return 0;
				} else if (locDiagSum == locRowSum) {
					strict = 0;
				}
			}
		}
	}
	Teuchos::reduceAll(*comm, Teuchos::REDUCE_MAX, 1, &strict, &totalStrict);
	if (totalStrict == 1) {
		//*fos << "col diagonal dominance:2" << std::endl;
		return 2;
	} else {
		//*fos << "col diagonal dominance:1" << std::endl;
		return 1;
	}	
}

ST calcDiagonalMean(const RCP<MAT> &A) {
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

// indicates the diagonal sign pattern
// -2 all negative, -1 nonpositive, 0 all zero, 1 nonnegative, 2 all positive, 
// 3 some negative,some or no zero,some positive
int calcDiagonalSign(const RCP<MAT> &A) {
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

size_t calcDiagonalNonzeros(const RCP<MAT> &A) {
	return A->getGlobalNumDiags();
}

size_t calcLowerBandwidth(const RCP<MAT> &A) {
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

// based off tinyurl.com/ktlpsah
RCP<MV> calcEigenValues(const RCP<MAT> &A, std::string eigenType) {
  Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
  RCP<NT> node = platform.getNode();

  //  Get norm
  ST mat_norm = A->getFrobeniusNorm();

  //  Start block Arnoldi iteration
  int nev = 1;
  int blockSize = 1;
  int numBlocks = 10*nev / blockSize;
  ST tol = 1e-6;

  //  Create parameters to pass to the solver
  Teuchos::ParameterList MyPL;
  //MyPL.set("Verbosity", verbosity);
  MyPL.set("Block Size", blockSize );                 // Add blockSize vectors to the basis per iteration
  MyPL.set("Convergence Tolerance", tol);   // How small do the residuals have to be
  MyPL.set("Relative Convergence Tolerance", false);  // Don't scale residuals by eigenvalues (when checking for convergence)
  MyPL.set("Use Locking", true);                      // Use deflation
  MyPL.set("Relative Locking Tolerance", false);      // Don't scale residuals by eigenvalues (when checking whether to lock a vector)
  MyPL.set("Num Blocks", numBlocks);                   // Maximum number of blocks in the subspace

  //  Default to largest magnitude 
  if (eigenType.compare("SM") == 0) {
    MyPL.set("Which", "SM");
  } else if (eigenType.compare("SR") == 0) {
    MyPL.set("Which", "SR");
  } else if (eigenType.compare("LR") == 0) {
    MyPL.set("Which", "LR");
  } else {
    MyPL.set("Which", "LM");
  }

  //  Create multivector for a initial vector to start the solver
  RCP<MV> ivec = rcp (new MV(A->getRowMap(), blockSize));
  MVT::MvRandom(*ivec);

  //  Create eigenproblem
  RCP<Anasazi::BasicEigenproblem<ST, MV, OP> > MyProblem = 
    rcp(new Anasazi::BasicEigenproblem<ST, MV, OP>(A, ivec));

  MyProblem->setHermitian(false);
  MyProblem->setNEV(nev);

  //  We are done with giving it info
  MyProblem->setProblem();

  //  Initialize TraceMin-Davidson Solver
  Anasazi::BlockKrylovSchurSolMgr<ST, MV, OP> MySolverMgr(MyProblem, MyPL);

  //  Solve the problem
  Anasazi::ReturnType returnCode = MySolverMgr.solve();
  if (returnCode != Anasazi::Converged) {
    *fos << "unconverged" << ", ";
  } 

  //  Get the results
  Anasazi::Eigensolution<ST, MV> sol = MyProblem->getSolution();
  std::vector<Anasazi::Value<ST> > evals = sol.Evals;
  RCP<MV> evecs = sol.Evecs;
  int numev = sol.numVecs;

  //  Compute residual 
  if (numev > 0) {
    Teuchos::SerialDenseMatrix<int, ST> T(numev,numev);
    for (int i = 0; i < numev; i++) {
      T(i,i) = evals[i].realpart;
    }
    std::vector<ST> normR(sol.numVecs);
    MV Kvec(A->getRowMap(), MVT::GetNumberVecs(*evecs));
    OPT::Apply(*A, *evecs, Kvec);
    MVT::MvTimesMatAddMv(-1.0, *evecs, T, 1.0, Kvec);
    MVT::MvNorm(Kvec, normR);
    for (int i=0; i<numev; i++) {
      *fos << evals[i].realpart << ", " << normR[i]/mat_norm << ", ";
    }
  }
	return evecs;  
}

void calcSmallestEigenvalues(const RCP<MAT> &A, std::string filename) {
	*fos << std::endl << "In function" << std::endl;
	Epetra_MpiComm eComm(MPI_COMM_WORLD);	

	int space_dim = 2;
  
  // Size of each of the dimensions of the domain
  std::vector<double> brick_dim( space_dim );
  brick_dim[0] = 1.0;
  brick_dim[1] = 1.0;
  
  // Number of elements in each of the dimensions of the domain
  std::vector<int> elements( space_dim );
  elements[0] = 10;
  elements[1] = 10;
  
  // Create problem
  RCP<ModalProblem> testCase = rcp( 
  	new ModeLaplace2DQ2(eComm, brick_dim[0], elements[0], brick_dim[1], elements[1]) );
  
  // Get the stiffness and mass matrices
 // RCP<Epetra_CrsMatrix> L = rcp( const_cast<Epetra_CrsMatrix *>(testCase->getStiffness()), false);
//RCP<Epetra_CrsMatrix> L = rcp( const_cast<Epetra_CrsMatrix *>(testCase->getStiffness()), true ); BREAKS

//  Create Matrix
  *fos << "test0" << std::endl;
	//TODO: Do without making redundant RCP of matrix
	Epetra_CrsMatrix* K;
	EpetraExt::MatrixMarketFileToCrsMatrix(filename.c_str(), eComm, K);
	RCP<Epetra_CrsMatrix> L = rcp(const_cast<Epetra_CrsMatrix*> (K), false);
	if (K == NULL) {
		*fos << "K is null" << std::endl;
	}
	if (L.is_null()) {
		*fos << "L is null" << std::endl;
	}	
//  Construct Ifpack preconditioner
	Teuchos::ParameterList ifpackList;
	Ifpack Factory;
	std::string PrecType = "ICT";
	int overlapLevel = 0;
	*fos << "Test before preconditioner" << std::endl;
	RCP<Ifpack_Preconditioner> Prec =	rcp( Factory.Create(PrecType, &*L, overlapLevel) );
	*fos << "before assert" << std::endl;
	assert(Prec != Teuchos::null);
	*fos << "after assert" << std::endl;
	ifpackList.set("fact: drop tolerance", 1e-4);
	ifpackList.set("fact: ict level-of-fill", 0.);
	ifpackList.set("schwarz: combine mode", "Add");
	Prec->SetParameters(ifpackList);
	Prec->Initialize();
	Prec->Compute();	
*fos << "test1" << std::endl;
//  Set up Belos block CG operator for inner iteration
	int blockSize = 3;
	int maxits = L->NumGlobalRows();
	RCP<Belos::LinearProblem<ST, Epetra_MultiVector, Epetra_Operator> > 
		My_LP = rcp(new Belos::LinearProblem<ST, Epetra_MultiVector, Epetra_Operator>());
	My_LP->setOperator(L); // only accepts RCP of crsmatrix
	RCP<Epetra_Operator> belosPrec = rcp (new Epetra_InvOperator (Prec.get()));
	My_LP->setLeftPrec(belosPrec);

	RCP<Teuchos::ParameterList> My_List = rcp(new Teuchos::ParameterList() );
	My_List->set( "Solver", "BlockCG" );
  My_List->set( "Maximum Iterations", maxits );
  My_List->set( "Block Size", 1 );
  My_List->set( "Convergence Tolerance", 1e-12 );

  RCP<Belos::EpetraOperator> BelosOp = 
  	rcp(new Belos::EpetraOperator (My_LP, My_List));

//  Start the block Arnoldi iteration
  double tol = 1.0e-8;
  int nev = 10;
  int numBlocks = 3*nev/blockSize;
  int maxRestarts = 5;
  //int step = 5;
  std::string which = "LM";
  int verbosity = Anasazi::Errors + Anasazi::Warnings + Anasazi::FinalSummary;
  
  // Create parameter list to pass into solver
  Teuchos::ParameterList MyPL;
  MyPL.set( "Verbosity", verbosity );
  MyPL.set( "Which", which );
  MyPL.set( "Block Size", blockSize );
  MyPL.set( "Num Blocks", numBlocks );
  MyPL.set( "Maximum Restarts", maxRestarts );
  MyPL.set( "Convergence Tolerance", tol );
  //MyPL.set( "Step Size", step );

*fos << "test2" << std::endl;
  RCP<Epetra_MultiVector> ivec = 
  	rcp(new Epetra_MultiVector(L->Map(), blockSize));
  Anasazi::MultiVecTraits<ST, Epetra_MultiVector>::MvRandom(*ivec);
  *fos << "testa" << std::endl;
  RCP<Anasazi::EpetraGenOp> Aop = 
  	rcp(new Anasazi::EpetraGenOp(BelosOp, L, false));
  RCP<Anasazi::BasicEigenproblem<ST, Epetra_MultiVector, Epetra_Operator> >	MyProblem = 
  	rcp(new Anasazi::BasicEigenproblem<ST,Epetra_MultiVector,Epetra_Operator> (Aop, L, ivec));
  MyProblem->setNEV(nev);
  MyProblem->setProblem();
  *fos << "testb" << std::endl;
  Anasazi::BlockKrylovSchurSolMgr<ST, Epetra_MultiVector, Epetra_Operator> MySolverMgr(MyProblem, MyPL);
  *fos << "testc" << std::endl;
  Anasazi::ReturnType returnCode = MySolverMgr.solve(); //breaks here
  *fos << "testd" << std::endl;
  if (returnCode != Anasazi::Converged) {
    *fos << "Anasazi::EigensolverMgr::solve() returned unconverged." << std::endl;
  }
  // Get the eigenvalues and eigenvectors from the eigenproblem

*fos << "test3" << std::endl;
  Anasazi::Eigensolution<ST,Epetra_MultiVector> sol = MyProblem->getSolution();
  std::vector<Anasazi::Value<ST> > evals = sol.Evals;
  Teuchos::RCP<Epetra_MultiVector> evecs = sol.Evecs;
  int numev = sol.numVecs;

}
