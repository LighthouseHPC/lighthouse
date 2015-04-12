#include "tpetra_solvers.h"

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int numNodes;
int myRank;

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

  RCP<MAT> A = Reader::readSparseFile(filename, comm, node, true);
  belosSolve(A);
}

//  https://code.google.com/p/trilinos/wiki/Tpetra_Belos_CreateSolver
void belosSolve(const RCP<const MAT> A) {
  RCP<ParameterList> solverParams = parameterList();
  solverParams->set("Num Blocks", 40); //max # of Krylov vectors to store
  solverParams->set("Maximum Iterations", 400); //total # of iters incl restarts
  solverParams->set("Convergence Tolerance", 1.0e-8); //relative residual 2-norm

  //  Create solver and specify algorithm
  Belos::SolverFactory<ST, MV, OP> belosFactory;
  RCP<Belos::SolverManager<ST, MV, OP> > solver = 
    belosFactory.create("GMRES", solverParams); 

  //  Create the preconditioner
  RCP<PT> prec;
  Ifpack2::Factory ifpack2Factory;
  const std::string precondType = "ILUT";
  prec = ifpack2Factory.create(precondType, A);
  prec->initialize();
  prec->compute();


  //  Create the linear problem
  RCP<MV> X = rcp (new MV (A->getDomainMap(), 1));
  RCP<MV> B = rcp (new MV (A->getRangeMap(), 1));
  B->randomize();
  typedef Belos::LinearProblem<ST, MV, OP> PT;
  RCP<PT> problem = rcp (new PT(A, X, B));
  problem->setRightPrec(prec);
  problem->setProblem();

  solver->setProblem(problem);
  Belos::ReturnType result = solver->solve();
  const int numIters = solver->getNumIters();
  if (result == Belos::Converged) {
    *fos << "The Belos solve took " << numIters << " iteration(s) to reach "
      "a relative residual tolerance of " << 1.0e-8 << "." << std::endl;
  } else {
    *fos << "The Belos solve took " << numIters << " iteration(s), but did not reach "
      "a relative residual tolerance of " << 1.0e-8 << "." << std::endl;
  }
}