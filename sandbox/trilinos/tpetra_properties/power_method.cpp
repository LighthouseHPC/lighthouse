#include <Tpetra_CrsMatrix.hpp>
#include <Tpetra_DefaultPlatform.hpp>
#include <Tpetra_Map.hpp>
#include <Tpetra_MultiVector.hpp>
#include <Tpetra_Vector.hpp>
#include <Tpetra_Version.hpp>
#include <Teuchos_Array.hpp>
#include <Teuchos_GlobalMPISession.hpp>
#include <Teuchos_oblackholestream.hpp>
#include <Teuchos_ScalarTraits.hpp>
#include <Teuchos_RCP.hpp>
#include <MatrixMarket_Tpetra.hpp>

template <class TpetraOperatorType>
class PowerMethod {
public:
  typedef typename TpetraOperatorType::scalar_type scalar_type;
  typedef typename TpetraOperatorType::local_ordinal_type local_ordinal_type;
  typedef typename TpetraOperatorType::global_ordinal_type global_ordinal_type;
  typedef typename TpetraOperatorType::node_type node_type;
  // The type of a Tpetra vector with the same template parameters as
  // those of TpetraOperatorType.
  typedef Tpetra::Vector<scalar_type, local_ordinal_type,
                         global_ordinal_type, node_type> vec_type;
  // The type of the norm of the above Tpetra::Vector specialization.
  typedef typename vec_type::mag_type magnitude_type;
  // Run the power method and return the eigenvalue estimate.
  //
  // Input arguments:
  //
  // A: The sparse matrix or operator, as a Tpetra::Operator.
  // niters: Maximum number of iterations of the power method.
  // tolerance: If the 2-norm of the residual A*x-lambda*x (for the
  //   current eigenvalue estimate lambda) is less than this, stop
  //   iterating.  The complicated expression for the type ensures that
  //   if the type of entries in the matrix A (scalar_type) is complex,
  //   then we'll be using a real-valued type ("magnitude") for the
  //   tolerance.  (You can't compare complex numbers using less than,
  //   so you can't test for convergence using a complex number.)
  // out: output stream to which to print the current status of the
  //   power method.
  static scalar_type
  run (const TpetraOperatorType& A,
       const int niters,
       const magnitude_type tolerance,
       std::ostream& out)
  {
    using std::endl;
    typedef Teuchos::ScalarTraits<scalar_type> STS;
    typedef Teuchos::ScalarTraits<magnitude_type> STM;
    // Create three vectors for iterating the power method.  Since the
    // power method computes z = A*q, q should be in the domain of A and
    // z should be in the range.  (Obviously the power method requires
    // that the domain and the range are equal, but it's a good idea to
    // get into the habit of thinking whether a particular vector
    // "belongs" in the domain or range of the matrix.)  The residual
    // vector "resid" is of course in the range of A.
    vec_type q (A.getDomainMap ());
    vec_type z (A.getRangeMap ());
    vec_type resid (A.getRangeMap ());
    // Fill the iteration vector z with random numbers to start.
    // Don't have grand expectations about the quality of our
    // pseudorandom number generator, but it is usually good enough
    // for eigensolvers.
    z.randomize ();
    // lambda: Current approximation of the eigenvalue of maximum magnitude.
    // normz: 2-norm of the current iteration vector z.
    // residual: 2-norm of the current residual vector 'resid'.
    //
    // Teuchos::ScalarTraits defines what zero and one means for any
    // type.  Most number types T know how to turn a 0 or a 1 (int)
    // into a T.  I have encountered some number types in C++ that do
    // not.  These tend to be extended-precision types that define
    // number operators and know how to convert from a float or
    // double, but don't have conversion operators for int.  Thus,
    // using Teuchos::ScalarTraits makes this code maximally general.
    scalar_type lambda = STS::zero ();
    magnitude_type normz = STM::zero ();
    magnitude_type residual = STM::zero ();
    const scalar_type one  = STS::one ();
    const scalar_type zero = STS::zero ();
    // How often to report progress in the power method.  Reporting
    // progress requires computing a residual, which can be expensive.
    // However, if you don't compute the residual often enough, you
    // might keep iterating even after you've converged.
    const int reportFrequency = 10;
    // Do the power method, until the method has converged or the
    // maximum iteration count has been reached.
    for (int iter = 0; iter < niters; ++iter) {
      normz = z.norm2 ();       // Compute the 2-norm of z
      q.scale (one / normz, z); // q := z / normz
      A.apply (q, z);           // z := A * q
      lambda = q.dot (z);       // Approx. max eigenvalue
      // Compute and report the residual norm every reportFrequency
      // iterations, or if we've reached the maximum iteration count.
      if (iter % reportFrequency == 0 || iter + 1 == niters) {
        resid.update (one, z, -lambda, q, zero); // z := A*q - lambda*q
        residual = resid.norm2 (); // 2-norm of the residual vector
        out << "Iteration " << iter << ":" << endl
            << "- lambda = " << lambda << endl
            << "- ||A*q - lambda*q||_2 = " << residual << endl;
      }
      if (residual < tolerance) {
        out << "Converged after " << iter << " iterations" << endl;
        break;
      } else if (iter+1 == niters) {
        out << "Failed to converge after " << niters << " iterations" << endl;
        break;
      }
    }
    return lambda;
  }
};


int main(int argc, char *argv[]) {
  using Teuchos::Array;
  using Teuchos::ArrayView;
  using Teuchos::ArrayRCP;
  using Teuchos::arcp;
  using Teuchos::RCP;
  using Teuchos::rcp;
  using Teuchos::tuple;
  using std::cerr;
  using std::cout;
  using std::endl;
  typedef Tpetra::Map<> map_type;
  typedef Tpetra::Vector<>::scalar_type scalar_type;
  typedef Tpetra::Vector<>::local_ordinal_type local_ordinal_type;
  typedef Tpetra::Vector<>::global_ordinal_type global_ordinal_type;
  typedef Tpetra::Vector<>::mag_type magnitude_type;
  typedef Tpetra::CrsMatrix<> crs_matrix_type;
  Teuchos::oblackholestream blackhole;
  Teuchos::GlobalMPISession mpiSession (&argc, &argv, &blackhole);
  RCP<const Teuchos::Comm<int> > comm =
    Tpetra::DefaultPlatform::getDefaultPlatform ().getComm ();
  const size_t myRank = comm->getRank();
  //const size_t numProcs = comm->getSize();
  // Make an output stream (for verbose output) that only prints on
  // Proc 0 of the communicator.
  Teuchos::oblackholestream blackHole;
  std::ostream& out = (myRank == 0) ? std::cout : blackHole;
  // Print the current version of Tpetra.
  out << Tpetra::version () << endl << endl;
  // The number of rows and columns in the matrix.
  const Tpetra::global_size_t numGblIndices = 50;
  // Construct a Map that puts approximately the same number of
  // equations on each processor.
  const global_ordinal_type indexBase = 0;
  RCP<const map_type> map =
    rcp (new map_type (numGblIndices, indexBase, comm));
  const size_t numMyElements = map->getNodeNumElements ();
  // If you like, you may get the list of global indices that the
  // calling process owns.  This is unnecessary if you don't mind
  // converting local indices to global indices.
  //
  // ArrayView<const global_ordinal_type> myGlobalElements =
  //   map->getNodeElementList ();
  out << endl << "Creating the sparse matrix" << endl;
  // Create a Tpetra sparse matrix whose rows have distribution given by the Map.
 
  std::string filename(argv[1]);
  RCP<Tpetra::Map<>::node_type> node = Tpetra::DefaultPlatform::getDefaultPlatform().getNode();
  RCP<crs_matrix_type> A = Tpetra::MatrixMarket::Reader<crs_matrix_type>::readSparseFile(
    filename, comm, node, true); 



  // Number of iterations
  const int niters = 500;
  // Desired (absolute) residual tolerance
  const magnitude_type tolerance = 1.0e-2;
  // Run the power method and report the result.
  scalar_type lambda =
    PowerMethod<crs_matrix_type>::run (*A, niters, tolerance, out);
  out << endl << "Estimated max eigenvalue: " << lambda << endl;
}