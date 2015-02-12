#include <Teuchos_Array.hpp>
#include <Teuchos_ScalarTraits.hpp>
#include <Teuchos_OrdinalTraits.hpp>
#include <Teuchos_RCP.hpp>
#include <Teuchos_GlobalMPISession.hpp>
#include <Teuchos_oblackholestream.hpp>
#include "Tpetra_DefaultPlatform.hpp"
#include "Tpetra_Version.hpp"
#include "Tpetra_Map.hpp"
#include "Tpetra_MultiVector.hpp"
#include "Tpetra_Vector.hpp"
#include "Tpetra_CrsMatrix.hpp"

int main(int argc, char* argv[]) {
	using Tpetra::global_size_t;
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

	typedef double scalar_type;
	typedef int local_ordinal_type;
	typedef long global_ordinal_type;

	typedef Teuchos::ScalarTraits<scalar_type>::magnitudeType magnitude_type;

	Teuchos::oblackholestream blackhole;
	Teuchos::GlobalMPISession mpiSession(&argc, &argv, &blackhole);
	RCP<const Teuchos::Comm<int> > comm = Tpetra::DefaultPlatform::getDefaultPlatform().getComm();

	const size_t myRank = comm->getRank();
	const size_t numProcs = comm->getSize();

	Teuchos::oblackholestream blackHole;
	std::ostream& out = (myRank == 0) ? std::cout : blackHole;

	// Print the current version of tpetra
	out << Tpetra::version() << endl << endl;


}
