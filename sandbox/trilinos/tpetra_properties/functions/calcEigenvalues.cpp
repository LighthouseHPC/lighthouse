#include "tpetra_properties_crsmatrix.h"

// based off tinyurl.com/ktlpsah
void calcEigenValues(const RCP<MAT> &A, std::string eigenType) {
	TimeMonitor LocalTimerLM (*timeEigenValuesLM);
	TimeMonitor LocalTimerSM (*timeEigenValuesSM);
	TimeMonitor LocalTimerLR (*timeEigenValuesLR);
	TimeMonitor LocalTimerSR (*timeEigenValuesSR);
	timeEigenValuesLM->stop();
	timeEigenValuesSM->stop();
	timeEigenValuesLR->stop();
	timeEigenValuesSR->stop();

  Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
  RCP<NT> node = platform.getNode();

  //  Get norm
  ST mat_norm = A->getFrobeniusNorm();

  //  Start block Arnoldi iteration
  int nev = 1;
  int blockSize = 1;
  int numBlocks = 10*nev / blockSize;
  ST tol = 1e-8;

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
  if (eigenType.compare("SR") == 0) {
    MyPL.set("Which", "SR");
    timeEigenValuesSR->start();
  } else if (eigenType.compare("SM") == 0) {
    MyPL.set("Which", "SM");
    timeEigenValuesSM->start();
  } else if (eigenType.compare("LR") == 0) {
    MyPL.set("Which", "LR");
    timeEigenValuesLR->start();
  } else {
    MyPL.set("Which", "LM");
    timeEigenValuesLM->start();
  }

  //  Create multivector for a initial vector to start the solver
  RCP<MV> ivec = rcp (new MV(A->getRowMap(), blockSize));
  MVT::MvRandom(*ivec);

  //  Create eigenproblem
  RCP<Anasazi::BasicEigenproblem<ST, MV, OP> > MyProblem = 
    rcp(new Anasazi::BasicEigenproblem<ST, MV, OP>(A, ivec));
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
    return;
  } 

  //  Get the results
  Anasazi::Eigensolution<ST, MV> sol = MyProblem->getSolution();
  std::vector<Anasazi::Value<ST> > evals = sol.Evals;
  for (int i = 0; i < 1; i++) {
  	*fos << evals[i].realpart <<  " : " << evals[i].imagpart << ", ";
  }
}