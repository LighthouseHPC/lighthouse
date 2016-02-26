#include "tpetra_solvers.h"

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
     // Print to screen
        std::cout << "No output directory was specified. Printing to screen" << std::endl;
        fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));
        unsigned found = filename.find_last_of("/\\");
        filename = filename.substr(found+1);
    } else { 
    // Print to file
        unsigned found = filename.find_last_of("/\\");
        std::string outputFilename = outputDir + "/" + filename.substr(found+1)+".out";
        std::cout << "Printing to " << outputFilename << std::endl;
        filename = filename.substr(found+1);
        outputFile.open(outputFilename.c_str());
        fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(outputFile));
        outputFile << "Matrix: " << filename << std::endl;
        outputFile << "Procs: " << comm->getSize() << std::endl;
    }
    // Do all the work
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

    //  Solving linear system with all prec/solver pairs
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
                *fos << solverIter << ", " << precIter << ", Error selecting prec/solver" << std::endl;
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
            } catch(...) {
                *fos << solverIter << ", " << precIter << ", Error creating linear problem" << std::endl;
            }
            try {
                //  Solve the linear problem 
                Belos::ReturnType result = solver->solve();
                timer.stop();
                *fos <<  solverIter  << ", "  << precIter; // output solver/prec pair
                if (result == result) {
                    *fos << ", converged, "; 
                } else {
                    *fos << ", unconverged, ";
                } 
                *fos << solver->getNumIters() << ", " << timer.totalElapsedTime() << std::endl;
            } catch (...) {
                *fos << solverIter << ", " << precIter << ", Error solving linear problem" << std::endl;
            }
        }
    }
}

