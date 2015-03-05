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
#include "AnasaziConfigDefs.hpp"
#include "AnasaziTraceMinDavidsonSolMgr.hpp"
#include "AnasaziBasicEigenproblem.hpp"
#include "AnasaziTpetraAdapter.hpp"
#include "AnasaziOperator.hpp"

//  Tpetra typedefs
typedef double ST;
typedef int LO;
typedef int64_t GO;
typedef Tpetra::DefaultPlatform::DefaultPlatformType Platform;
//typedef Tpetra::DefaultPlatform::DefaultPlatformType::NodeType NT;
typedef Tpetra::Map<>::node_type NT;
typedef Tpetra::CrsMatrix<ST, LO, GO, NT> MAT;
typedef Tpetra::MultiVector<ST, LO, GO, NT> MV;
typedef Tpetra::MatrixMarket::Reader<MAT> Reader;
typedef Tpetra::Operator<ST, LO, GO, NT> OP;

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
void calcRowVariance(const RCP<MAT> &A);
void calcColVariance(const RCP<MAT> &A);
void calcDiagVariance(const RCP<MAT> &A);
void calcNonzeros(const RCP<MAT> &A);
void calcDim(const RCP<MAT> &A);
void calcFrobeniusNorm(const RCP<MAT> &A);
void calcSymmetricFrobeniusNorm(const RCP<MAT> &A);
void calcAntisymmetricFrobeniusNorm(const RCP<MAT> &A);
void calcInfNorm(const RCP<MAT> &A);
void calcOneNorm(const RCP<MAT> &A);
void calcSymmetricInfNorm(const RCP<MAT> &A);
void calcAntisymmetricInfNorm(const RCP<MAT> &A);
void calcMaxNonzerosPerRow(const RCP<MAT> &A);
void calcMinNonzerosPerRow(const RCP<MAT> &A);
void calcAvgNonzerosPerRow(const RCP<MAT> &A);
void calcTrace(const RCP<MAT> &A);
void calcAbsTrace(const RCP<MAT> &A);
void calcDummyRows(const RCP<MAT> &A);
//void calcDummyRowsKind(const RCP<MAT> &A);
void calcNonzeroPatternSymmetryPercentage(const RCP<MAT> &A);
void calcNumericalSymmetryPercentage(const RCP<MAT> &A);
void calcNumericalSymmetryPercentageMPI(const RCP<MAT> &A);
void calcNonzeroPatternSymmetry(const RCP<MAT> &A);
void calcNumericalSymmetry(const RCP<MAT> &A);
void calcRowDiagonalDominance(const RCP<MAT> &A);
void calcColDiagonalDominance(const RCP<MAT> &A);
void calcDiagonalMean(const RCP<MAT> &A);

void calcDiagonalSign(const RCP<MAT> &A);
void calcDiagonalNonzeros(const RCP<MAT> &A);
void calcLowerBandwidth(const RCP<MAT> &A);
void calcUpperBandwidth(const RCP<MAT> &A);
void calcBandwidth(const RCP<MAT> &A);

void calcLambdaMaxByMagnitudeReal(const RCP<MAT> &A, int argc, char *argv[]);

