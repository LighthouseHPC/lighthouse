#include <Tpetra_CrsMatrix.hpp>
#include <Tpetra_DefaultPlatform.hpp>
#include <Tpetra_Version.hpp>
#include <BelosTpetraAdapter.hpp>
#include <BelosSolverFactory.hpp>
#include <Ifpack2_Factory.hpp>
#include <Teuchos_GlobalMPISession.hpp>
#include <Teuchos_oblackholestream.hpp>
#include <Teuchos_TimeMonitor.hpp>
#include <iostream>

// Solve the linear system(s) AX=B, using GMRES with the preconditioner M.
//
// B: right-hand side(s) of the linear system(s) AX=B.
// X: On input: initial guess(es) for solving AX=B.  On output: solution vector(s).
// A: the sparse matrix (or operator; it need not be a sparse matrix).
// M: if not null, the (right) preconditioner for A.
//
// In this example, MV is a specialization of Tpetra::MultiVector, 
// and OP is a specialization of Tpetra::Operator (the parent class 
// of Tpetra::CrsMatrix, Ifpack2::Preconditioner, and other classes 
// as well).
template<class MV, class OP>
void
solve (std::ostream& out, MV& X, const MV& B, const OP& A, Teuchos::RCP<OP> M) 
{
  using Teuchos::ParameterList;
  using Teuchos::parameterList;
  using Teuchos::RCP; 
  using Teuchos::rcp;
  using Teuchos::rcpFromRef; // Make a "weak" RCP from a reference.
  typedef typename MV::scalar_type scalar_type;

  // Make an empty new parameter list.
  RCP<ParameterList> solverParams = parameterList();

  // Set some GMRES parameters.
  //
  // "Num Blocks" = Maximum number of Krylov vectors to store.  This
  // is also the restart length.  "Block" here refers to the ability
  // of this particular solver (and many other Belos solvers) to solve
  // multiple linear systems at a time, even though we may only be
  // solving one linear system in this example.
  //
  // "Maximum Iterations": Maximum total number of iterations,
  // including restarts.
  //
  // "Convergence Tolerance": By default, this is the relative
  // residual 2-norm, although you can change the meaning of the
  // convergence tolerance using other parameters.
  solverParams->set ("Num Blocks", 40);
  solverParams->set ("Maximum Iterations", 400);
  solverParams->set ("Convergence Tolerance", 1.0e-8);

  // Create the GMRES solver using a "factory" and 
  // the list of solver parameters created above.
  Belos::SolverFactory<scalar_type, MV, OP> factory;
  RCP<Belos::SolverManager<scalar_type, MV, OP> > solver = 
    factory.create ("GMRES", solverParams);

  // Create a LinearProblem struct with the problem to solve.
  // A, X, B, and M are passed by (smart) pointer, not copied.
  typedef Belos::LinearProblem<scalar_type, MV, OP> problem_type;
  RCP<problem_type> problem = 
    rcp (new problem_type (rcpFromRef (A), rcpFromRef (X), rcpFromRef (B)));
  // You don't have to call this if you don't have a preconditioner.
  // If M is null, then Belos won't use a (right) preconditioner.
  problem->setRightPrec (M);
  // Tell the LinearProblem to make itself ready to solve.
  problem->setProblem ();

  // Tell the solver what problem you want to solve.
  solver->setProblem (problem);

  // Attempt to solve the linear system.  result == Belos::Converged 
  // means that it was solved to the desired tolerance.  This call 
  // overwrites X with the computed approximate solution.
  Belos::ReturnType result = solver->solve();

  // Ask the solver how many iterations the last solve() took.
  const int numIters = solver->getNumIters();

  if (result == Belos::Converged) {
    out << "The Belos solve took " << numIters << " iteration(s) to reach "
      "a relative residual tolerance of " << 1.0e-8 << "." << std::endl;
  } else {
    out << "The Belos solve took " << numIters << " iteration(s), but did not reach "
      "a relative residual tolerance of " << 1.0e-8 << "." << std::endl;
  }
}

// Get the Ifpack2 preconditioner type and its parameter list.
// You may modify this function to change the preconditioner type
// and its parameters.
//
// The first output argument is the preconditioner name.  In this
// case, it's "ILUT", for Saad's ILUT incomplete factorization
// preconditioner.
//
// The second output argument is the parameter list for the
// preconditioner.  Give it to the preconditioner's setParameters()
// method.  The parameter list this function returns tells ILUT to
// use fill level 2, drop tolerance 0, and absolute threshold 0.1.
//
// Note that with Ifpack2, the type of preconditioner is separate
// from the ParameterList for that preconditioner.
void 
getPrecondTypeAndParameters (std::string& precondType, Teuchos::ParameterList& pl)
{
  using Teuchos::ParameterList;

  // The name of the type of preconditioner to use.
  precondType = "ILUT";

  // Ifpack2 expects arguments of type 'double' here, regardless of
  // the scalar or magnitude types of the entries of the sparse
  // matrix.
  const double fillLevel = 2.0;
  const double dropTol = 0.0;
  const double absThreshold = 0.1;

  pl.set ("fact: ilut level-of-fill", fillLevel);
  pl.set ("fact: drop tolerance", dropTol);
  pl.set ("fact: absolute threshold", absThreshold);
}

// This function encapsulates creation of an Ifpack2 preconditioner
// from a Tpetra::CrsMatrix.  It returns the preconditioner as a
// Tpetra::Operator, which is the parent class of
// Ifpack2::Preconditioner.
//
// The template parameter TpetraMatrixType must be a specialization of
// Tpetra::CrsMatrix.  We template this function on the matrix type,
// rather than on the five template arguments of Tpetra::CrsMatrix,
// because CrsMatrix has some nice typedefs that let us retrieve those
// template arguments.  It's easier to template on one thing than on
// five things!  Recall that if T is a template parameter, and if T is
// a class with a typedef inside T::U, then you have to use "typename"
// to get at the typedef U.
//
// You don't have to use this function to make an Ifpack2
// preconditioner.  We just find it easier to read the code example if
// we wrap up the preconditioner creation in its own little function.
template<class TpetraMatrixType>
Teuchos::RCP<Tpetra::Operator<typename TpetraMatrixType::scalar_type,
                              typename TpetraMatrixType::local_ordinal_type,
                              typename TpetraMatrixType::global_ordinal_type,
                              typename TpetraMatrixType::node_type> >
createPreconditioner (const Teuchos::RCP<const TpetraMatrixType>& A,
                      const std::string& precondType,
                      const Teuchos::ParameterList& plist,
                      std::ostream& out,
                      std::ostream& err)
{
  using Teuchos::ParameterList;
  using Teuchos::RCP;
  using Teuchos::rcp;
  using Teuchos::Time;
  using Teuchos::TimeMonitor;
  using std::endl;

  // Fetch the typedefs defined by Tpetra::CrsMatrix.
  typedef typename TpetraMatrixType::scalar_type scalar_type;
  typedef typename TpetraMatrixType::local_ordinal_type local_ordinal_type;
  typedef typename TpetraMatrixType::global_ordinal_type global_ordinal_type;
  typedef typename TpetraMatrixType::node_type node_type;

  // Ifpack2's generic Preconditioner interface implements
  // Tpetra::Operator.  A Tpetra::Operator is an abstraction of a
  // function mapping a (Multi)Vector to a (Multi)Vector, with the
  // option of applying the transpose or conjugate transpose of the
  // operator.  Tpetra::CrsMatrix implements Operator as well.
  typedef Tpetra::Operator<scalar_type, local_ordinal_type, 
                           global_ordinal_type, node_type> op_type;

  // These are just some convenience typedefs.
  typedef Teuchos::ScalarTraits<scalar_type> STS;
  typedef typename STS::magnitudeType magnitude_type;
  typedef Teuchos::ScalarTraits<magnitude_type> STM;

  // An Ifpack2::Preconditioner is-a Tpetra::Operator.  Ifpack2
  // creates a Preconditioner object, but users of iterative methods
  // want a Tpetra::Operator.  That's why create() returns an Operator
  // instead of a Preconditioner.
  typedef Ifpack2::Preconditioner<scalar_type, local_ordinal_type, 
                                  global_ordinal_type, node_type> prec_type;

  // Create timers to show how long it takes for Ifpack2 to do various operations.
  RCP<Time> initTimer = TimeMonitor::getNewCounter ("Ifpack2::Preconditioner::initialize");
  RCP<Time> computeTimer = TimeMonitor::getNewCounter ("Ifpack2::Preconditioner::compute");
  RCP<Time> condestTimer = TimeMonitor::getNewCounter ("Ifpack2::Preconditioner::condest");

  err << "Creating ILUT preconditioner" << endl 
      << "-- Configuring" << endl;
  //
  // Create the preconditioner and set parameters.
  //
  // This doesn't actually _compute_ the preconditioner.
  // It just sets up the specific type of preconditioner and
  // its associated parameters (which depend on the type).
  //
  RCP<prec_type> prec;
  Ifpack2::Factory factory;
  // Set up the preconditioner of the given type.
  prec = factory.create (precondType, A);
  prec->setParameters (plist);

  err << "-- Initializing" << endl;
  {
    TimeMonitor mon (*initTimer);
    prec->initialize();
  }

  // THIS ACTUALLY COMPUTES THE PRECONDITIONER
  // (e.g., does the incomplete factorization).
  err << "-- Computing" << endl;
  {
    TimeMonitor mon (*computeTimer);
    prec->compute();
  }

  if (precondType != "RELAXATION") {
    err << "-- Estimating condition number" << endl;
    magnitude_type condest = STM::one();
    {
      TimeMonitor mon (*condestTimer);
      condest = prec->computeCondEst (Ifpack2::Cheap);
    }
    out << endl << "Ifpack2 preconditioner's estimated condition number: " << condest << endl;
  }
  return prec;
}

// Create and return a simple example CrsMatrix.
template<class TpetraMatrixType>
Teuchos::RCP<const TpetraMatrixType>
createMatrix (const Teuchos::RCP<const Teuchos::Comm<int> >& comm,
              const Teuchos::RCP<typename TpetraMatrixType::node_type>& node)
{
  using Teuchos::arcp;
  using Teuchos::ArrayRCP;
  using Teuchos::ArrayView;
  using Teuchos::RCP;
  using Teuchos::rcp;
  using Teuchos::Time;
  using Teuchos::TimeMonitor;
  using Teuchos::tuple;

  typedef TpetraMatrixType matrix_type;

  // Fetch the timer for sparse matrix creation.
  //
  // If you are using Trilinos 10.6 instead of the development branch
  // (10.7), just create a new timer here, and remove the bit in
  // main() that creates a timer.
  //
  RCP<Time> timer = TimeMonitor::lookupCounter ("Sparse matrix creation");
  if (timer.is_null())
    timer = TimeMonitor::getNewCounter ("Sparse matrix creation");

  // Time the whole scope of this routine, not counting timer lookup.
  TimeMonitor monitor (*timer);

  // Fetch typedefs from the Tpetra::CrsMatrix.
  typedef typename TpetraMatrixType::scalar_type scalar_type;
  typedef typename TpetraMatrixType::local_ordinal_type local_ordinal_type;
  typedef typename TpetraMatrixType::global_ordinal_type global_ordinal_type;
  typedef typename TpetraMatrixType::node_type node_type;

  // The type of the Tpetra::Map that describes how the matrix is distributed.
  typedef Tpetra::Map<local_ordinal_type, global_ordinal_type, node_type> map_type;

  // The global number of rows in the matrix A to create.  We scale
  // this relative to the number of (MPI) processes, so that no matter
  // how many MPI processes you run, every process will have 10 rows.
  const Tpetra::global_size_t numGlobalElements = 10 * comm->getSize();

  // Construct a Map that puts approximately the same number of
  // equations on each processor.
  const global_ordinal_type indexBase = 0;
  RCP<const map_type > map = 
    rcp (new map_type (numGlobalElements, indexBase, comm, 
                       Tpetra::GloballyDistributed, node));

  // Get update list and the number of equations that this MPI process
  // owns.
  const size_t numMyElements = map->getNodeNumElements();
  ArrayView<const global_ordinal_type> myGlobalElements = map->getNodeElementList();

  // NumNz[i] will be the number of nonzero entries for the i-th
  // global equation on this MPI process.
  ArrayRCP<size_t> NumNz = arcp<size_t> (numMyElements);

  // We are building a tridiagonal matrix where each row is (-1 2 -1),
  // so we need 2 off-diagonal terms (except for the first and last
  // equation).
  for (size_t i = 0; i < numMyElements; ++i) {
    if (myGlobalElements[i] == 0 || static_cast<Tpetra::global_size_t>(myGlobalElements[i]) == numGlobalElements-1) {
      NumNz[i] = 2; // First or last equation
    } else {
      NumNz[i] = 3;
    }
  }

  // Create a Tpetra::Matrix using the Map, with a static allocation
  // dictated by NumNz.  (We know exactly how many elements there will
  // be in each row, so we use static profile for efficiency.)
  RCP<matrix_type> A = rcp (new matrix_type (map, NumNz, Tpetra::StaticProfile));

  // We are done with NumNZ; free it.
  NumNz = Teuchos::null;

  // Add rows one at a time.  Off diagonal values will always be -1.
  const scalar_type two    = static_cast<scalar_type> ( 2.0);
  const scalar_type negOne = static_cast<scalar_type> (-1.0);

  for (size_t i = 0; i < numMyElements; i++) {
    if (myGlobalElements[i] == 0) {
      A->insertGlobalValues (myGlobalElements[i], 
                             tuple (myGlobalElements[i], myGlobalElements[i]+1),
                             tuple (two, negOne));
    }
    else if (static_cast<Tpetra::global_size_t> (myGlobalElements[i]) == numGlobalElements-1) {
      A->insertGlobalValues (myGlobalElements[i],
                             tuple (myGlobalElements[i]-1, myGlobalElements[i]),
                             tuple (negOne, two));
    }
    else {
      A->insertGlobalValues (myGlobalElements[i],
                             tuple (myGlobalElements[i]-1, myGlobalElements[i], myGlobalElements[i]+1),
                             tuple (negOne, two, negOne));
    }
  }

  // Finish up the matrix.
  A->fillComplete ();
  return A;
}

int 
main (int argc, char *argv[]) 
{
  using std::endl;
  using Teuchos::RCP;
  using Teuchos::rcp;
  using Teuchos::ParameterList;
  using Teuchos::Time;
  using Teuchos::TimeMonitor;

  Teuchos::oblackholestream blackHole;
  Teuchos::GlobalMPISession mpiSession (&argc, &argv, &blackHole);
  RCP<const Teuchos::Comm<int> > comm = 
    Tpetra::DefaultPlatform::getDefaultPlatform().getComm();

  // Set up Tpetra typedefs.
  typedef double scalar_type;
  typedef int local_ordinal_type;
  typedef long global_ordinal_type;
  typedef Kokkos::DefaultNode::DefaultNodeType node_type;

  // Create the Kokkos Node instance.  
  // Tpetra objects use this object for intranode parallel operations.
  RCP<node_type> node = Kokkos::DefaultNode::getDefaultNode ();

  const int myRank = comm->getRank();
  const int numProcs = comm->getSize();
  std::ostream& out = (myRank == 0) ? std::cout : blackHole;
  std::ostream& err = (myRank == 0) ? std::cerr : blackHole;

  // Make a timer for sparse matrix creation.
  //
  // If you are using Trilinos 10.6 instead of the development branch
  // (10.7), just delete this line of code, and make the other change
  // mentioned above.
  RCP<Time> sparseMatrixCreationTimer = 
    TimeMonitor::getNewCounter ("Sparse matrix creation");

  // Run the whole example: create the sparse matrix, and compute the preconditioner.
  // Print out the Tpetra software version information.
  out << Tpetra::version() << endl << endl;

  typedef Tpetra::CrsMatrix<scalar_type, local_ordinal_type, global_ordinal_type, node_type> matrix_type;
  typedef Tpetra::Operator<scalar_type, local_ordinal_type, global_ordinal_type, node_type> op_type;
  typedef Tpetra::MultiVector<scalar_type, local_ordinal_type, global_ordinal_type, node_type> vec_type;


  // Create an example sparse matrix.
  RCP<const matrix_type> A = createMatrix<matrix_type> (comm, node);

  // Get the preconditioner type and its parameter list.  Modify the
  // definition of that function if you want to use a different
  // preconditioner or parameters.
  std::string precondType;
  ParameterList plist;
  getPrecondTypeAndParameters (precondType, plist);

  // Compute the preconditioner using the matrix A.
  // The matrix A itself is not modified.
  RCP<op_type> M = createPreconditioner<matrix_type> (A, precondType, plist, out, err);

  // Create vectors ("multivectors" may store one or more vectors).
  RCP<vec_type> X = rcp (new vec_type (A->getDomainMap (), 1)); // Set to zeros by default.
  RCP<vec_type> B = rcp (new vec_type (A->getRangeMap (), 1));
  B->randomize ();

  // Solve the linear system using Belos.
  solve<vec_type, op_type> (out, *X, *B, *A, M);

  // Summarize global performance timing results, for all timers
  // created using TimeMonitor::getNewCounter().
  TimeMonitor::summarize (out);

  return 0;
}
