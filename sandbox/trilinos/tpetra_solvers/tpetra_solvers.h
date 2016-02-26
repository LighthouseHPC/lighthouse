//  Tpetra
#include <Tpetra_CrsMatrix.hpp>
#include <MatrixMarket_Tpetra.hpp>

//  Belos
#include <BelosTpetraAdapter.hpp>
#include <BelosSolverFactory.hpp>

//  Ifpack2
#include <Ifpack2_Factory.hpp>

//  Typedefs
typedef double ST;
typedef int LO;
typedef int64_t GO;
typedef Tpetra::DefaultPlatform::DefaultPlatformType Platform;
typedef Tpetra::Map<>::node_type NT;
typedef Tpetra::CrsMatrix<ST, LO, GO, NT> MAT;
typedef Tpetra::MultiVector<ST, LO, GO, NT> MV;
typedef Tpetra::MatrixMarket::Reader<MAT> Reader;
typedef Tpetra::Operator<ST, LO, GO, NT> OP;
typedef Tpetra::Vector<ST, LO, GO, NT> VEC;
typedef Teuchos::RCP<Teuchos::Time> TIMER;
typedef Teuchos::ScalarTraits<ST> STS;
typedef STS::magnitudeType magnitude_type;
typedef Teuchos::ScalarTraits<magnitude_type> STM;
typedef Ifpack2::Preconditioner<ST, LO, GO, NT> PRE;
typedef Belos::LinearProblem<ST, MV, OP> LP;
typedef Belos::SolverManager<ST, MV, OP> BSM;

//  Namespaces
using Tpetra::global_size_t;
using Tpetra::Map;
using Tpetra::Import;
using Teuchos::RCP;
using Teuchos::rcp;
using Teuchos::rcpFromRef;
using Teuchos::ArrayView;
using Teuchos::Array;
using Teuchos::Time;
using Teuchos::TimeMonitor;
using Teuchos::ParameterList;
using Teuchos::parameterList;

//  Globals
const	std::vector<std::string> ifpack2Precs = {"ILUT", "RILUK", "DIAGONAL",
	"RELAXATION", "CHEBYSHEV"};
const std::vector<std::string> belosSolvers = {"Block GMRES", "CG", "CGPoly", 
	"Flexible GMRES", "GMRES", "GmresPoly", "Pseudo Block CG", "Pseudo Block GMRES", 
	"PseudoBlockCG", "PseudoBlockGmres", "Recycling CG", "Recycling GMRES", "Seed CG", 
	"Seed GMRES", "Stochastic CG", "Transpose-Free QMR", "Block CG", "Block GMRES", 
	"GCRODR", "Hybrid Block GMRES", "MINRES", "PCPG", "Pseudoblock CG", 
	"Pseudoblock GMRES", "Pseudoblock Stochastic CG", "RCG", "TFQMR"};

//  Functions
void belosSolve(const RCP<const MAT> &A, const std::string &filename);
RCP<PRE> getIfpack2Preconditoner(const RCP<const MAT> &A, std::string ifpack2PrecChoice);

RCP<const Teuchos::Comm<int> > comm;
RCP<Teuchos::FancyOStream> fos;
int numNodes;
int myRank;
