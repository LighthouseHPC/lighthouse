#include "tpetra_properties_crsmatrix.h"

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int numNodes;
int myRank;

TIMER timeRowVariance;
TIMER timeColVariance;
TIMER timeDiagVariance;
TIMER timeNonzeros;
TIMER timeDim;
TIMER timeFrobeniusNorm;
TIMER timeSymmetricFrobeniusNorm;
TIMER timeAntisymmetricFrobeniusNorm;
TIMER timeOneNorm;
TIMER timeInfNorm;
TIMER timeSymmetricInfNorm;
TIMER timeAntisymmetricInfNorm;
TIMER timeMaxNonzerosPerRow;
TIMER timeMinNonzerosPerRow;
TIMER timeAvgNonzerosPerRow;
TIMER timeTrace;
TIMER timeAbsTrace;
TIMER timeDummyRows;
TIMER timeSymmetry;
TIMER timeRowDiagonalDominance;
TIMER timeColDiagonalDominance;
TIMER timeLowerBandwidth;
TIMER timeUpperBandwidth;
TIMER timeDiagonalMean;
TIMER timeDiagonalSign;
TIMER timeDiagonalNonzeros;
TIMER timeEigenValuesLM;
TIMER timeEigenValuesSM;
TIMER timeEigenValuesLR;
TIMER timeEigenValuesSR; 

int main(int argc, char *argv[]) {
	std::string filename(argv[1]);
  if (filename.empty()) {
  	*fos << "No .mtx file was specified" << std::endl;
  	return -1;
  }	
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

  RCP<MAT> A = Reader::readSparseFile(filename, comm, node, true);
  Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
	RCP<MAT> B = transposer.createTranspose();
	initTimers();
  runGauntlet(A);
  TimeMonitor::summarize(out);
  //calcSmallestEigenvalues(A, filename);
  //calcInverseMethod(A);
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
	*fos << "\nLM:" << std::endl;
  calcEigenValues(A, "LM");
  *fos << "\nSM:" << std::endl;
  calcEigenValues(A, "SM");
  *fos << "\nLR:" << std::endl;
  calcEigenValues(A, "LR");
  *fos << "\nSR:" << std::endl;
  calcEigenValues(A, "SR"); 
  *fos << std::endl;
  
}

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
  int nev = 4;
  int blockSize = 1;
  int numBlocks = 10*nev / blockSize;
  ST tol = 1e-6;

  //  Create parameters to pass to the solver
  Teuchos::ParameterList MyPL;
  //MyPL.set("Verbosity", verbosity);
  MyPL.set("Block Size", blockSize );                 // Add blockSize vectors to the basis per iteration
  MyPL.set("Convergence Tolerance", tol);   // How small do the residuals have to be
  //MyPL.set("Relative Convergence Tolerance", false);  // Don't scale residuals by eigenvalues (when checking for convergence)
  //MyPL.set("Use Locking", true);                      // Use deflation
  //MyPL.set("Relative Locking Tolerance", false);      // Don't scale residuals by eigenvalues (when checking whether to lock a vector)
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

  //  Taken from https://github.com/qsnake/trilinos/blob/master/packages/tpetra/example/HybridPlatform/build_eigproblem.hpp
  //  Create preconditioner
	typedef Ifpack2::Preconditioner<ST,LO,GO,NT> prec_type;	
	Ifpack2::Factory factory;
	const std::string precName = "KRYLOV";
	RCP<prec_type> prec = factory.create(precName, (RCP<const MAT>)A);
	Teuchos::ParameterList factoryParams;
	prec->setParameters(factoryParams);  
  prec->initialize();
  prec->compute();

  //  Finish setting up the eigenproblem
  MyProblem->setPrec(prec);
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
	
}

void initTimers() {
	timeRowVariance = TimeMonitor::getNewCounter("Row Variance");
	timeColVariance = TimeMonitor::getNewCounter("Col Variance");
	timeDiagVariance = TimeMonitor::getNewCounter("Diag Variance");
	timeNonzeros = TimeMonitor::getNewCounter("Nonzeros");
	timeDim = TimeMonitor::getNewCounter("Dimension");
	timeFrobeniusNorm = TimeMonitor::getNewCounter("Frob. Norm");
	timeSymmetricFrobeniusNorm = TimeMonitor::getNewCounter("Symm Frob Norm");
	timeAntisymmetricFrobeniusNorm = TimeMonitor::getNewCounter("Antisymm Frob Norm");
	timeOneNorm = TimeMonitor::getNewCounter("One Norm");
	timeInfNorm = TimeMonitor::getNewCounter("Inf Norm");
	timeSymmetricInfNorm = TimeMonitor::getNewCounter("Symm Inf Norm");
	timeAntisymmetricInfNorm = TimeMonitor::getNewCounter("Antisymm Inf Norm");
	timeMaxNonzerosPerRow = TimeMonitor::getNewCounter("Max Nonzeros / Row");
	timeMinNonzerosPerRow = TimeMonitor::getNewCounter("Min Nonzeros / Row");
	timeAvgNonzerosPerRow = TimeMonitor::getNewCounter("Avg Nonzeros / Row");
	timeTrace = TimeMonitor::getNewCounter("Trace");
	timeAbsTrace = TimeMonitor::getNewCounter("Abs Trace");
	timeDummyRows = TimeMonitor::getNewCounter("Dummy Rows");
	timeSymmetry = TimeMonitor::getNewCounter("Symmetry");
	timeRowDiagonalDominance = TimeMonitor::getNewCounter("Row Diag Dominance");
	timeColDiagonalDominance = TimeMonitor::getNewCounter("Col Diag Dominance");
	timeLowerBandwidth = TimeMonitor::getNewCounter("Lower Bandwidth");
	timeUpperBandwidth = TimeMonitor::getNewCounter("Upper Bandwidth");
	timeDiagonalMean = TimeMonitor::getNewCounter("Diagonal Mean");
	timeDiagonalSign = TimeMonitor::getNewCounter("Diagonal Sign");
	timeDiagonalNonzeros = TimeMonitor::getNewCounter("Diadonal Nonzeros");
	timeEigenValuesLM = TimeMonitor::getNewCounter("Eigen LM");
	timeEigenValuesSM = TimeMonitor::getNewCounter("Eigen SM");
	timeEigenValuesLR = TimeMonitor::getNewCounter("Eigen LR");
	timeEigenValuesSR = TimeMonitor::getNewCounter("Eigen SR"); 
}