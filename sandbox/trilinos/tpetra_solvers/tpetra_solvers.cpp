#include "tpetra_solvers.h"

int main(int argc, char *argv[]) {
    double program_start, program_end;
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
    belosSolvers = determineSolvers(filename);
    Teuchos::GlobalMPISession mpiSession(&argc, &argv);
    Platform& platform = Tpetra::DefaultPlatform::getDefaultPlatform();
    comm = platform.getComm();
    RCP<NT> node = platform.getNode();
    myRank = comm->getRank();

    mpiSession.barrier();
    program_start = MPI_Wtime();
    RCP<MAT> A = Reader::readSparseFile(filename, comm, node, true); 
    Teuchos::oblackholestream blackhole;
    std::ostream& out = (myRank == 0) ? std::cout : blackhole;
    std::ofstream outputFile;

    //  How to output results
    if (outputDir.empty()) { 
     // Print to screen
        fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(out));
        *fos << "No output directory was specified. Printing to screen" << std::endl;
        unsigned found = filename.find_last_of("/\\");
        filename = filename.substr(found+1);
    } else { 
    // Print to file
        unsigned found = filename.find_last_of("/\\");
        std::string outputFilename = outputDir + "/" + filename.substr(found+1)+".out";
        if (myRank == 0)
            std::cout << "Printing to " << outputFilename << std::endl;
        filename = filename.substr(found+1);
        outputFile.open(outputFilename.c_str());
        fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(outputFile));
        fos = Teuchos::fancyOStream(Teuchos::rcpFromRef(outputFile));
    }
    // Do all the work
    belosSolve(A, filename);
    program_end = MPI_Wtime();
    *fos << "Total Time: " << program_end - program_start << std::endl;
    return 0;
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
    Teuchos::Time overall_timer("overall_timer", false);
    RCP<PRE> prec;
    RCP<BSM> solver; 
    Belos::SolverFactory<ST, MV, OP> belosFactory;
    
    overall_timer.start(true);
    //  Solving linear system with all prec/solver pairs
    for (auto solverIter : belosSolvers) {
        for (auto precIter : ifpack2Precs) {
            timer.start(true);
            solver = Teuchos::null;
            prec = Teuchos::null;
            try {
                solver = getBelosSolver(A, solverIter); 
                if (precIter.compare("None"))
                    prec = getIfpack2Preconditoner(A, precIter);
            } catch (const std::exception &exc) {
                *fos << solverIter << ", " << precIter << ", Error selecting prec/solver, ";
                *fos << timer.totalElapsedTime() << std::endl;
                if (myRank == 0)
                    std::cerr << exc.what() << std::endl;
            }
            *fos << filename << ", " << comm->getSize() << ", ";
            try {
                //  Create the x and randomized b multivectors
                RCP<MV> x = rcp (new MV (A->getDomainMap(), 1));
                RCP<MV> b = rcp (new MV (A->getRangeMap(), 1));
                b->randomize();

                //  Create the linear problem
                RCP<LP> problem = rcp (new LP(A, x, b));
                if (precIter.compare("None"))
                    problem->setLeftPrec(prec);
                problem->setProblem(); //done adding to the linear problem
                solver->setProblem(problem); //add the linear problem to the solver
            } catch(const std::exception &exc) {
                *fos << solverIter << ", " << precIter << ", Error creating linear problem, ";
                *fos << timer.totalElapsedTime() << std::endl;
                if (myRank == 0)
                    std::cerr << exc.what() << std::endl;
            }
            try {
                //  Solve the linear problem 
                Belos::ReturnType result = solver->solve();
                timer.stop();
                *fos <<  std::string(solverIter)  << ", "  << precIter; // output solver/prec pair
                if (result == Belos::Converged) {
                    *fos << ", converged, "; 
                } else {
                    *fos << ", unconverged, ";
                } 
                *fos << solver->getNumIters() << ", " << timer.totalElapsedTime() << std::endl;
            } catch (const std::exception &exc) {
                *fos << solverIter << ", " << precIter << ", Error solving linear problem, ";
                *fos << timer.totalElapsedTime() << std::endl;
                if (myRank == 0)
                    std::cerr << exc.what() << std::endl;
            }
        }
    }
    overall_timer.stop();
    *fos << "Time to solve all permutations (Trilinos): " << overall_timer.totalElapsedTime() << std::endl;
}

STRINGS determineSolvers(const std::string &filename) {
    std::ifstream file(filename);
    std::string firstLine, firstNumbers;
    unsigned int rows, cols;
    if (file.good()) {
        std::getline(file, firstLine);
        std::getline(file, firstNumbers);
        while (firstNumbers.find("%") == 0) {
            std::getline(file, firstNumbers);
        }
        std::stringstream ss(firstNumbers);
        ss >> rows >> cols;
    }
    file.close();
    if (firstLine.find("symmetric") != std::string::npos){ // include all
        std::cout << "In sym\n";
        return belos_all;  
    } else if (firstLine.find("general") != std::string::npos) { // only include sq+rec
        std::cout << "In gen\n";
        return belos_sq;        
    } else {
        std::cout << "Should never be here\n";
        exit(-1);
    }
}
