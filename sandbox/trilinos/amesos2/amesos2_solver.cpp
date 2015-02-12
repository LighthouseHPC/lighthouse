#include <Teuchos_ScalarTraits.hpp>
#include <Teuchos_RCP.hpp>
#include <Teuchos_GlobalMPISession.hpp>
#include <Teuchos_oblackholestream.hpp>
#include <Teuchos_VerboseObject.hpp>
#include <Teuchos_CommandLineProcessor.hpp>
#include <Tpetra_DefaultPlatform.hpp>
#include <Tpetra_Map.hpp>
#include <Tpetra_MultiVector.hpp>
#include <Tpetra_CrsMatrix.hpp>
// I/O for Matrix-Market files
#include <MatrixMarket_Tpetra.hpp>
#include <Tpetra_Import.hpp>
#include "Amesos2.hpp"
#include "Amesos2_Version.hpp"
int main(int argc, char *argv[]) {
	Teuchos::GlobalMPISession mpiSession(&argc,&argv);
	typedef double Scalar;
	typedef Teuchos::ScalarTraits<Scalar>::magnitudeType Magnitude;
	typedef int Ordinal;
	typedef double Scalar;
	typedef int LO;
	typedef int GO;
	typedef Tpetra::DefaultPlatform::DefaultPlatformType           Platform;
	typedef Tpetra::DefaultPlatform::DefaultPlatformType::NodeType Node;
	typedef Tpetra::CrsMatrix<Scalar,LO,GO,Node> MAT;
	typedef Tpetra::MultiVector<Scalar,LO,GO,Node> MV;
	using Tpetra::global_size_t;
	using Tpetra::Map;
	using Tpetra::Import;
	using Teuchos::RCP;
	using Teuchos::rcp;
	// 
	// Get the default communicator
	//
	Platform &platform = Tpetra::DefaultPlatform::getDefaultPlatform();
	Teuchos::RCP<const Teuchos::Comm<int> > comm = platform.getComm();
	Teuchos::RCP<Node>                      node = platform.getNode();
	int myRank = comm->getRank();
	Teuchos::oblackholestream blackhole;
	bool printMatrix   = false;
	bool printSolution = false;
	bool printTiming   = false;
	bool allprint      = false;
	bool verbose = (myRank==0);
	std::string filename("../demo.mtx");
	Teuchos::CommandLineProcessor cmdp(false,true);
	cmdp.setOption("verbose","quiet",&verbose,"Print messages and results.");
	cmdp.setOption("filename",&filename,"Filename for Matrix-Market test matrix.");
	cmdp.setOption("print-matrix","no-print-matrix",&printMatrix,"Print the full matrix after reading it.");
	cmdp.setOption("print-solution","no-print-solution",&printSolution,"Print solution vector after solve.");
	cmdp.setOption("print-timing","no-print-timing",&printTiming,"Print solver timing statistics");
	cmdp.setOption("all-print","root-print",&allprint,"All processors print to out");
	if (cmdp.parse(argc,argv) != Teuchos::CommandLineProcessor::PARSE_SUCCESSFUL) {
		return -1;
	}
	std::ostream& out = ( (allprint || (myRank == 0)) ? std::cout : blackhole );
	RCP<Teuchos::FancyOStream> fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));
	// Say hello
	out << myRank << " : " << Amesos2::version() << std::endl << std::endl;
	const size_t numVectors = 1;
	RCP<MAT> A = Tpetra::MatrixMarket::Reader<MAT>::readSparseFile(filename, comm, node);
	if( printMatrix ){
		A->describe(*fos, Teuchos::VERB_EXTREME);
	}
	else if( verbose ){
		std::cout << std::endl << A->description() << std::endl << std::endl;
	}
	// get the maps
	RCP<const Map<LO,GO,Node> > dmnmap = A->getDomainMap();   
	RCP<const Map<LO,GO,Node> > rngmap = A->getRangeMap();
	GO nrows = dmnmap->getGlobalNumElements();
	RCP<Map<LO,GO,Node> > root_map
		= rcp( new Map<LO,GO,Node>(nrows,myRank == 0 ? nrows : 0,0,comm) );
	RCP<MV> Xhat = rcp( new MV(root_map,numVectors) );
	RCP<Import<LO,GO,Node> > importer = rcp( new Import<LO,GO,Node>(dmnmap,root_map) );
	// Create random X
	RCP<MV> X = rcp(new MV(dmnmap,numVectors));
	X->randomize();
	RCP<MV> B = rcp(new MV(rngmap,numVectors));
	//B->randomize();
	B->putScalar(10);

	// Constructor from Factory
	RCP<Amesos2::Solver<MAT,MV> > solver;

	solver = Amesos2::create<MAT,MV>("Lapack", A, X, B);
	solver->solve();
	if( 1 ){
		// Print the solution
		if( allprint ){
			if( myRank == 0 ) *fos << "Solution :" << std::endl;
			Xhat->describe(*fos,Teuchos::VERB_EXTREME);
			*fos << std::endl;
		} else {
			Xhat->doImport(*X,*importer,Tpetra::REPLACE);
			if( myRank == 0 ){
				*fos << "Solution :" << std::endl;
				Xhat->describe(*fos,Teuchos::VERB_EXTREME);
				*fos << std::endl;
			}
		}
	}
//	if( printTiming ){
		// Print some timing statistics
		solver->printTiming(*fos);
//	}

	// We are done.
	return 0;
}
