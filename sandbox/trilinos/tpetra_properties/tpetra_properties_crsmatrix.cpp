#include "tpetra_properties_crsmatrix.h"

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int myRank, numNodes;

TIMER timeRowVariance, timeColVariance, timeDiagVariance,
timeNonzeros,timeDim,timeFrobeniusNorm,timeSymmetricFrobeniusNorm,
timeAntisymmetricFrobeniusNorm,timeOneNorm,timeInfNorm,timeSymmetricInfNorm,
timeAntisymmetricInfNorm,timeMaxNonzerosPerRow,timeMinNonzerosPerRow,
timeAvgNonzerosPerRow,timeTrace,timeAbsTrace,timeDummyRows,
timeSymmetry,timeRowDiagonalDominance,timeColDiagonalDominance,
timeLowerBandwidth,timeUpperBandwidth,timeDiagonalMean,timeDiagonalSign,
timeDiagonalNonzeros,timeEigenValuesLM,timeEigenValuesSM,timeEigenValuesLR,
timeEigenValuesSR;

int main(int argc, char *argv[]) {
	std::string outputDir;
	if (argv[1] == NULL) {
		std::cout << "No input file was specified" << std::endl;
		return -1;
	} 
	if (argv[2] != NULL) {
		outputDir = argv[2];	
	}
	std::string origFilename = argv[1];
	std::string filename = origFilename;
	

	//  General setup for Teuchos/communication
	Teuchos::GlobalMPISession mpiSession(&argc, &argv);
	Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
  comm = platform.getComm();
  RCP<NT> node = platform.getNode();
  myRank = comm->getRank();
  Teuchos::oblackholestream blackhole;
  std::ostream& out = (myRank == 0) ? std::cout : blackhole;
  std::ofstream outputFile;
  bool complex = false;

  //  Decide to print to screen or file
  if (outputDir.empty()) { 
  	std::cout << "No output directory was specified. Printing to screen" << std::endl;
	  fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));
	  unsigned found = filename.find_last_of("/\\");
	  filename = filename.substr(found+1);
  } else { //  print to file
  	unsigned found = filename.find_last_of("/\\");
	  std::string outputFilename = outputDir + "/" + filename.substr(found+1)+".out";
	  filename = filename.substr(found+1);
	  outputFile.open(outputFilename.c_str());
	  fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(outputFile));
	  
	}

	//  Check if matrix is complex
	std::ifstream infile;
	infile.open(argv[1]);
	if (infile.is_open()) {
		std::string firstLine;
		getline(infile, firstLine);
		if (firstLine.find("complex") != std::string::npos) {
			complex = true;
		}
	} 
	infile.close();	
	if (complex) {
		RCP<MATC> A = ReaderC::readSparseFile(origFilename, comm, node, true); 
	  Tpetra::RowMatrixTransposer<STC, LO, GO, NT> transposer(A);	
		RCP<MATC> B = transposer.createTranspose();
		*fos << "complex" << std::endl;
		*fos << "Matrix: " << filename << std::endl;
  	*fos << "Procs: " << comm->getSize() << std::endl;
		initTimers();
	  runGauntlet(A);
	} else {
		RCP<MAT> A = Reader::readSparseFile(origFilename, comm, node, true); 
	  Tpetra::RowMatrixTransposer<ST, LO, GO, NT> transposer(A);	
		RCP<MAT> B = transposer.createTranspose();
		*fos << "real" << std::endl;
		*fos << "Matrix: " << filename << std::endl;
	  *fos << "Procs: " << comm->getSize() << std::endl;
		initTimers();
	  runGauntlet(A);
	}

	//  Output timing results
  if (outputDir.empty()) {
	  TimeMonitor::report(out);
	} else {
		RCP<Teuchos::ParameterList> reportParams = Teuchos::parameterList();
	  reportParams->set("Report format", "YAML");
	  reportParams->set("YAML style", "compact");
	  TimeMonitor::report(comm.ptr(), outputFile, "", reportParams);
	}
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
	/*
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
  Values(A, "LM");
  calcEigenValues(A, "LR");
  calcEigenValues(A, "SM");
  calcEigenValues(A, "SR"); 
  */
  *fos << std::endl;
}
void runGauntlet(const RCP<MATC> &A) {
	// Test squareness
	if (A->getGlobalNumRows() != A->getGlobalNumCols() ) {
		*fos << "Not a square matrix, exiting." << std::endl;
		exit(-1);
	}
	*fos << comm->getSize() << ", ";
	*fos << calcRowVariance(A) << ", ";
	*fos << calcColVariance(A) << ", ";
	*fos << calcDiagVariance(A) << ", ";
	/*
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
  calcEigenValues(A, "LR");
  calcEigenValues(A, "SM");
  calcEigenValues(A, "SR"); 
  */
  *fos << std::endl;
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
