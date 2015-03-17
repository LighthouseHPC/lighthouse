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
#include <Teuchos_Array.hpp>
#include <Tpetra_RowMatrixTransposer.hpp>
#include <Teuchos_CommHelpers.hpp>
#include <MatrixMarket_Tpetra.hpp>
#include <Tpetra_Import.hpp>
#include <TpetraExt_MatrixMatrix.hpp>
#include <cmath>
#include <Teuchos_ParameterList.hpp>
#include <stdint.h>
#include <Tpetra_Operator.hpp>
#include <Tpetra_Version.hpp>
#include <TpetraExt_MatrixMatrix_def.hpp>
#include <Tpetra_ConfigDefs.hpp>

//Anasazi
#include <AnasaziConfigDefs.hpp>
#include <AnasaziTraceMinDavidsonSolMgr.hpp>
#include <AnasaziGeneralizedDavidsonSolMgr.hpp>
#include <AnasaziBlockKrylovSchurSolMgr.hpp>
#include <AnasaziBasicEigenproblem.hpp>
#include <AnasaziTpetraAdapter.hpp>
#include <AnasaziOperator.hpp>

//Ifpack
#include <Ifpack.h>
#include <Ifpack_Preconditioner.h>

//  Tpetra typedefs
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

//  Anasazi typedefs
typedef Anasazi::MultiVecTraits<ST, MV> MVT;
typedef Anasazi::OperatorTraits<ST, MV, OP> OPT;

using Tpetra::global_size_t;
using Tpetra::Map;
using Tpetra::Import;
using Teuchos::RCP;
using Teuchos::rcp;
using Teuchos::ArrayView;
using Teuchos::Array;

void runGauntlet(const RCP<MAT> &A);

ST calcRowVariance(const RCP<MAT> &A);
ST calcColVariance(const RCP<MAT> &A);
ST calcDiagVariance(const RCP<MAT> &A);
size_t calcNonzeros(const RCP<MAT> &A);
size_t calcDim(const RCP<MAT> &A);
ST calcFrobeniusNorm(const RCP<MAT> &A);
ST calcSymmetricFrobeniusNorm(const RCP<MAT> &A);
ST calcAntisymmetricFrobeniusNorm(const RCP<MAT> &A);
ST calcInfNorm(const RCP<MAT> &A);
ST calcOneNorm(const RCP<MAT> &A);
ST calcSymmetricInfNorm(const RCP<MAT> &A);
ST calcAntisymmetricInfNorm(const RCP<MAT> &A);
size_t calcMaxNonzerosPerRow(const RCP<MAT> &A);
size_t calcMinNonzerosPerRow(const RCP<MAT> &A);
ST calcAvgNonzerosPerRow(const RCP<MAT> &A);
ST calcTrace(const RCP<MAT> &A);
ST calcAbsTrace(const RCP<MAT> &A);
size_t calcDummyRows(const RCP<MAT> &A);
std::vector<ST> calcSymmetry(const RCP<MAT> &A);
int calcRowDiagonalDominance(const RCP<MAT> &A);
int calcColDiagonalDominance(const RCP<MAT> &A);
ST calcDiagonalMean(const RCP<MAT> &A);
int calcDiagonalSign(const RCP<MAT> &A);
size_t calcDiagonalNonzeros(const RCP<MAT> &A);
size_t calcLowerBandwidth(const RCP<MAT> &A);
size_t calcUpperBandwidth(const RCP<MAT> &A);
RCP<MV> calcEigenValues(const RCP<MAT> &A, std::string eigenType);

void calcNonzeroPatternSymmetryPercentage(const RCP<MAT> &A);