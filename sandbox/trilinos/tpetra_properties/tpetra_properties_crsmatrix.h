#include <Teuchos_ScalarTraits.hpp>
#include <Teuchos_RCP.hpp>
#include <Teuchos_GlobalMPISession.hpp>
#include <Teuchos_oblackholestream.hpp>
#include <Teuchos_VerboseObject.hpp>
#include <Teuchos_CommandLineProcessor.hpp>
#include <Tpetra_DefaultPlatform.hpp>
#include <Tpetra_Map.hpp>
#include <Tpetra_Vector.hpp>
#include <Tpetra_MultiVector.hpp>
#include <Tpetra_CrsMatrix.hpp>
#include <Teuchos_ArrayView.hpp>
#include <Tpetra_RowMatrixTransposer.hpp>
#include <Teuchos_CommHelpers.hpp>
// I/O for Matrix-Market files
#include <MatrixMarket_Tpetra.hpp>
#include <Tpetra_Import.hpp>
#include <TpetraExt_MatrixMatrix.hpp>
//#include <Teuchos_DefaultMpiComm.hpp>
//#include <Teuchos_Comm.hpp>

// Typedefs given in Amesos2 example code
typedef double ST;
typedef int LO;
typedef long GO;
typedef Tpetra::DefaultPlatform::DefaultPlatformType::NodeType NT;
typedef Tpetra::DefaultPlatform::DefaultPlatformType Platform;
typedef Tpetra::CrsMatrix<ST, LO, GO, NT> MAT;
typedef Tpetra::Vector<ST, LO, GO, NT> VEC;
typedef Tpetra::Map<LO, GO, NT> MAP;

using Tpetra::global_size_t;
using Tpetra::Map;
using Tpetra::Import;
using Teuchos::RCP;
using Teuchos::rcp;
using Teuchos::ArrayView;

void runGauntlet(RCP<MAT> &A);
void calcRowVariance(RCP<MAT> &A, bool transpose);
void calcColVariance(RCP<MAT> &A);
void calcDiagVariance(RCP<MAT> &A);
void calcNonzeros(RCP<MAT> &A);
void calcDim(RCP<MAT> &A);
void calcFrobeniusNorm(RCP<MAT> &A);
void calcSymmetricFrobeniusNorm(RCP<MAT> &A);
void calcAntisymmetricFrobeniusNorm(RCP<MAT> &A);
void calcOneNorm(RCP<MAT> &A);
void calcInfNorm(RCP<MAT> &A);
void calcSymmetricInfNorm(RCP<MAT> &A);
void calcAntisymmetricInfNorm(RCP<MAT> &A);
void calcMaxNonzerosPerRow(RCP<MAT> &A);
void calcTrace(RCP<MAT> &A);
void calcAbsTrace(RCP<MAT> &A);
void calcMinNonzerosPerRow(RCP<MAT> &A);
void calcAvgNonzerosPerRow(RCP<MAT> &A);
void calcDummyRows(RCP<MAT> &A);
void calcDummyRowsKind(RCP<MAT> &A);
void calcNumericalSymmetry(RCP<MAT> &A);
void calcNonzeroPatternSymmetry(RCP<MAT> &A);
void calcNumericalValueSymmetry(RCP<MAT> &A);
void calcNonzeroPatternSymmetry(RCP<MAT> &A);
void calcRowDiagonalDominance(RCP<MAT> &A);
void calcColDiagonalDominance(RCP<MAT> &A);
void calcDiagonalAvg(RCP<MAT> &A);
void calcDiagonalSign(RCP<MAT> &A);
void calcDiagonalNonzeros(RCP<MAT> &A);
void calcLowerBandwidth(RCP<MAT> &A);
void calcUpperBandwidth(RCP<MAT> &A);

