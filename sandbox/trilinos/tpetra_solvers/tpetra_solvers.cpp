#include "tpetra_solvers.h"

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int numNodes;
int myRank;

int main(int argc, char *argv[]) {
	std::string outputDir;
	if (argv[1] == NULL) {
		std::cout << "No input file was specified" << std::endl;
		std::cout << "Usage: ./tpetra_solvers <.mtx file> [<output_dir>]" << std::endl;
		return -1;
	} 
	if (argv[2] != NULL) {
		outputDir = argv[2];	
	}
	std::string filename = argv[1];
	

	//  General setup for Teuchos/communication
	Teuchos::GlobalMPISession mpiSession(&argc, &argv);
	Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
  comm = platform.getComm();
  RCP<NT> node = platform.getNode();
  myRank = comm->getRank();
  RCP<MAT> A = Reader::readSparseFile(filename, comm, node, true); 
  Teuchos::oblackholestream blackhole;
  std::ostream& out = (myRank == 0) ? std::cout : blackhole;
  std::ofstream outputFile;

	//  How to output results
  if (outputDir.empty()) {
  	std::cout << "No output directory was specified. Printing to screen" << std::endl;
	  fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));
	  unsigned found = filename.find_last_of("/\\");
	  filename = filename.substr(found+1);
	} else {
		unsigned found = filename.find_last_of("/\\");
	  std::string outputFilename = outputDir + "/" + filename.substr(found+1)+".out";
	  filename = filename.substr(found+1);
		//std::cout << "outputFilename:" << outputFilename << std::endl;
	  outputFile.open(outputFilename.c_str());
	  fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(outputFile));
	  outputFile << "Matrix: " << filename << std::endl;
	  outputFile << "Procs: " << comm->getSize() << std::endl;
	}

  
  belosSolve(A, filename);
}

RCP<PRE> getIfpack2Preconditoner(const RCP<const MAT> &A, 
																 std::string ifpack2PrecChoice) 
{
	RCP<PRE> prec;
	Ifpack2::Factory ifpack2Factory;
	prec = ifpack2Factory.create(ifpack2PrecChoice, A);
	prec->initialize();
	prec->compute();
	return prec;
}

RCP<BSM> getBelosSolver(const RCP<const MAT> &A, 
												std::string belosSolverChoice) 
{
	Belos::SolverFactory<ST, MV, OP> belosFactory;
	RCP<ParameterList> solverParams = parameterList();
	solverParams->set ("Num Blocks", 40);
	solverParams->set ("Maximum Iterations", 1000);
	solverParams->set ("Convergence Tolerance", 1.0e-8);
	RCP<BSM> solver = belosFactory.create(belosSolverChoice, solverParams);
	return solver;
}

//  https://code.google.com/p/trilinos/wiki/Tpetra_Belos_CreateSolver
void belosSolve(const RCP<const MAT> &A, const std::string &filename) {
  // Create solver and preconditioner 
  Teuchos::Time timer("timer", false);
  RCP<PRE> prec;
  RCP<BSM> solver; 
  Belos::SolverFactory<ST, MV, OP> belosFactory;

  //  Create solver and preconditioner
  for (auto solverIter : belosSolvers) {
  	for (auto precIter : ifpack2Precs) {
  		timer.start(true);
  		solver = Teuchos::null;
  		prec = Teuchos::null;
  		*fos << filename << ", ";
		  try {
		  	solver = getBelosSolver(A, solverIter); 
		    prec = getIfpack2Preconditoner(A, precIter);
		  } catch (...) {
		  	*fos << solverIter << ", " << precIter << ", creation_error" << std::endl;
		  	continue;	
		  }
		  try {
			  //  Create the X and randomized B multivectors
			  RCP<MV> X = rcp (new MV (A->getDomainMap(), 1));
			  RCP<MV> B = rcp (new MV (A->getRangeMap(), 1));
			  B->randomize();

		  	//  Create the linear problem
			  RCP<LP> problem = rcp (new LP(A, X, B));
			  problem->setLeftPrec(prec);
			  problem->setProblem(); //done adding to the linear problem
			  solver->setProblem(problem); //add the linear problem to the solver

		  //  Solve the problem 
			  Belos::ReturnType result = solver->solve();
			  const int numIters = solver->getNumIters();
			  timer.stop();
			  *fos <<  solverIter  << ", "  << precIter;
			  if (result == Belos::Converged) {
			    *fos << ", converged, "; 
			  } else {
			  	*fos << ", unconverged, ";
			  } 
			  *fos << numIters << ", " << timer.totalElapsedTime() << std::endl;
			} catch (...) {
				*fos << solverIter << ", " << precIter << ", solve_error" << std::endl;
			}
		}
	}
}

bool calcSymmetry(const RCP<MAT> &A) {
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
				//  Matching indices found
				if (mapB.count (iterA->first) ) {
					//  Check if values for those indices match
					if ( iterA->second != mapB[iterA->first] ) {
						return false;	
					}
				}
			}
		}
	}
	return true;
}
