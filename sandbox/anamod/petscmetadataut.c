#include <stdlib.h>
#include "petscsnes.h"
#include "petscmetadataut.h"

#include "petsc-private/matimpl.h"
#include "petsc-private/snesimpl.h"
#include "anamod.h"
#include "anamodsalsamodules.h"

// #include "Profile/TauAPI.h"
// #include <TAU.h>

PETSCMETADATAOBJECT PMetadataObject;
char event[PETSC_MAX_PATH_LEN];
char gridsize[PETSC_MAX_PATH_LEN];
PetscBool metadatainit = 0;
PetscBool metadatafini = 0;

/*
    PetscMetadataInit - Initializes metadata library objects
*/
#undef __FUNCT__
#define __FUNCT__ PetscMetadataInit
PetscErrorCode PetscMetadataInit(void)
{

 PetscFunctionBegin;
  
 if (!metadatainit){

   PMetadataObject.num = 0;
   metadatainit = 1;
 }

 PetscFunctionReturn(0);
}

/*
    MetadataSetEvent - Sets the event name for metadata
*/
#undef __FUNCT__
#define __FUNCT__ MetadataSetEvent
PetscErrorCode MetadataSetEvent(char e[])
{

   PetscFunctionBegin;
   
   sprintf(event,"%s", e); 

   PetscFunctionReturn(0);
}

/*
    SNESMonitorMetadata - Collects and stores the metadata information for SNES solvers
*/
#undef __FUNCT__
#define __FUNCT__ SNESMonitorMetadata
PetscErrorCode SNESMonitorMetadata(SNES snes,PetscInt its,PetscReal fgnorm,void *dummy)
{
  PetscErrorCode ierr;
  SNESType snestype;
  KSP ksp;
  const char *ksptype;
  int index;
  PetscInt rows, cols, x, y, ilulevels=0, asmoverlap=1;
  PetscBool flg;
  PC pc;
  PCType pctype;
  KSPConvergedReason reason;
  PetscReal snesrtol, ksprtol;
  Mat A, B;

  PetscFunctionBegin;

  ierr = SNESGetType(snes,&snestype);CHKERRQ(ierr);
  ierr = SNESGetKSP(snes,&ksp);CHKERRQ(ierr);
  ierr = KSPGetType(ksp,&ksptype);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp, &pc);CHKERRQ(ierr);
  ierr = PCGetType(pc, &pctype); CHKERRQ(ierr);
  if (!strcmp(pctype,PCILU)) {
     PetscInt levels;
     ierr = PetscOptionsGetInt(PETSC_NULL,"-pc_factor_levels", &levels, &flg); CHKERRQ(ierr);
     if (flg) ilulevels = levels;
  }
  if (!strcmp(pctype,PCASM)) {
     PetscInt levels;
     ierr = PetscOptionsGetInt(PETSC_NULL,"-pc_asm_overlap", &levels, &flg); CHKERRQ(ierr);
     if (flg) asmoverlap = levels;
  }
  ierr = KSPGetOperators(ksp, &A, &B, PETSC_NULL); CHKERRQ(ierr);
  ierr = KSPGetOperators(ksp, &A, &B, PETSC_NULL); CHKERRQ(ierr);

  ierr = MatGetSize(snes->jacobian, &x, &y);CHKERRQ(ierr);
  ierr = PetscOptionsGetInt(PETSC_NULL,"-da_grid_x", &rows, PETSC_NULL); CHKERRQ(ierr);
  ierr = PetscOptionsGetInt(PETSC_NULL,"-da_grid_y", &cols, PETSC_NULL); CHKERRQ(ierr);
  sprintf(gridsize,"%dx%d", rows, cols);

  ierr  = KSPGetConvergedReason(ksp, &reason); CHKERRQ(ierr);

  ierr = SNESGetTolerances(snes,0,&snesrtol,0,0,0);CHKERRQ(ierr);

  ierr = KSPGetTolerances(ksp,&ksprtol,0,0,0);CHKERRQ(ierr);

  if(snestype){

    index = PMetadataObject.num;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionMethodNonlinearSolver");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "snes");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "string");
    sprintf(PMetadataObject.PetscMetadataTable[index].value, snestype);
    index++;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionMethodLinearSolver");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "ksp");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "string");
    sprintf(PMetadataObject.PetscMetadataTable[index].value, "%s", ksptype);
    index++;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionMethodPC");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "pc");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "string");
    if (!strcmp(pctype,PCILU)) 
      sprintf(PMetadataObject.PetscMetadataTable[index].value, "%s_%d", pctype, ilulevels);
    else if (!strcmp(pctype,PCASM)) 
      sprintf(PMetadataObject.PetscMetadataTable[index].value, "%s_%d", pctype, asmoverlap);
    else 
      sprintf(PMetadataObject.PetscMetadataTable[index].value, "%s", pctype);
    index++;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionProperty");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "fnorm");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "double");
    sprintf(PMetadataObject.PetscMetadataTable[index].value,"%14.12e", fgnorm);
    index++;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionProperty");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "matrixsize");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "string");
    sprintf(PMetadataObject.PetscMetadataTable[index].value,"%dx%d", x, y);
    index++;

    // Matrix properties
    PMetadataObject.num=index;
    //printf("Computing matrix properties\n");
    MatComputePropertiesVariance(A,event);
    index = PMetadataObject.num;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionProperty");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "snesrtol");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "double");
    sprintf(PMetadataObject.PetscMetadataTable[index].value,"%14.6e", snesrtol);
    index++;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionProperty");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "ksprtol");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "double");
    sprintf(PMetadataObject.PetscMetadataTable[index].value,"%14.6e", ksprtol);
    index++;

    sprintf(PMetadataObject.PetscMetadataTable[index].category, "SolutionProperty");
    sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
    sprintf(PMetadataObject.PetscMetadataTable[index].name, "success");
    sprintf(PMetadataObject.PetscMetadataTable[index].type, "bool");
    sprintf(PMetadataObject.PetscMetadataTable[index].value,"%d", (reason < 0)?0:1);
    index++;

  }

  PMetadataObject.num=index;

  PetscFunctionReturn(0);
}

/*
   PetscMetadataFini - Outputs the metadata into a text file in csv format.

#undef __FUNCT__
#define __FUNCT__ PetscMetadataFini
PetscErrorCode PetscMetadataFini(void)
{
   PetscErrorCode ierr;
   int counter, ts_counter;
   char programname[PETSC_MAX_PATH_LEN];
   FILE *metadatafd;
   char metadatafile[20];
   int numProcs;
   char t_metadata_name[PETSC_MAX_PATH_LEN];
   char t_metadata_value[PETSC_MAX_PATH_LEN]; 
   PassiveReal lidvelocity, grashof,kspmaxit;
   PetscInt gridx, gridy;
   PetscReal snesrtol, ksprtol;
   char ksptype[PETSC_MAX_PATH_LEN];
   char snestype[PETSC_MAX_PATH_LEN];
   char pctype[PETSC_MAX_PATH_LEN];
   
   PetscFunctionBegin;

   if (!metadatafini){

     metadatafini = 1;
     
     ts_counter = 0;

     ierr = PetscOptionsGetReal(PETSC_NULL,"-lidvelocity",&lidvelocity,PETSC_NULL);CHKERRQ(ierr);
     ierr = PetscOptionsGetReal(PETSC_NULL,"-grashof",&grashof,PETSC_NULL);CHKERRQ(ierr);

     ierr = PetscOptionsGetReal(PETSC_NULL,"-ksp_max_it",&kspmaxit,PETSC_NULL);CHKERRQ(ierr);
     if (! kspmaxit) kspmaxit=10000; 
     ierr = PetscOptionsGetInt(PETSC_NULL,"-da_grid_x", &gridx, PETSC_NULL); CHKERRQ(ierr);
     ierr = PetscOptionsGetInt(PETSC_NULL,"-da_grid_y", &gridy, PETSC_NULL); CHKERRQ(ierr);
     ierr = PetscOptionsGetReal(PETSC_NULL,"-snes_rtol",&snesrtol,PETSC_NULL);CHKERRQ(ierr);
     ierr = PetscOptionsGetReal(PETSC_NULL,"-ksp_rtol",&ksprtol,PETSC_NULL);CHKERRQ(ierr);
     ierr = PetscOptionsGetString(PETSC_NULL,"-snes_type",snestype,PETSC_MAX_PATH_LEN, PETSC_NULL);CHKERRQ(ierr);
     ierr = PetscOptionsGetString(PETSC_NULL,"-ksp_type",ksptype,PETSC_MAX_PATH_LEN, PETSC_NULL);CHKERRQ(ierr);

     ierr = PetscOptionsGetString(PETSC_NULL,"-pc_type",pctype,PETSC_MAX_PATH_LEN, PETSC_NULL);CHKERRQ(ierr);
   
     ierr = PetscGetProgramName(programname,PETSC_MAX_PATH_LEN);CHKERRQ(ierr);

     ierr = MPI_Comm_size(PETSC_COMM_WORLD,&numProcs);CHKERRQ(ierr);

     sprintf(metadatafile,"metadata.txt");

     metadatafd   = fopen(metadatafile,"w"); 

     if (!metadatafd) SETERRQ1(PETSC_ERR_FILE_OPEN,"Cannot open log file: %s",metadatafile,NULL);

     fprintf(metadatafd, "!%s, p%d, x%dy%d-lid%G-grh%G-srtol%1.2e-krtol%1.2e-snes%s-ksp%s-kspmaxit%G-pc%s\n", programname, numProcs, gridx, gridy, lidvelocity, grashof, snesrtol, ksprtol, snestype, ksptype, kspmaxit, pctype);

     //output program wide parameters

     fprintf(metadatafd,"NULL, TrialProperty, procs, int, %d;\n", numProcs);
     fprintf(metadatafd,"NULL, TrialProperty, gridsize, string, %s;\n", gridsize);
    
     sprintf(t_metadata_name, "gridsize");
     sprintf(t_metadata_value, "%s", gridsize);
     Tau_metadata(t_metadata_name, t_metadata_value);

     sprintf(t_metadata_name, "procs");
     sprintf(t_metadata_value, "%d", numProcs);
     Tau_metadata(t_metadata_name, t_metadata_value);

     for(counter = 0; counter < PMetadataObject.num; counter++)
       {
	 fprintf(metadatafd, "%s, ",PMetadataObject.PetscMetadataTable[counter].event);
	 fprintf(metadatafd, "%s, ",PMetadataObject.PetscMetadataTable[counter].category);
	 fprintf(metadatafd, "%s, ",PMetadataObject.PetscMetadataTable[counter].name);
	 fprintf(metadatafd, "%s, ",PMetadataObject.PetscMetadataTable[counter].type);
	 fprintf(metadatafd, "%s;\n",PMetadataObject.PetscMetadataTable[counter].value);
       }

     fclose(metadatafd);
   }

   PetscFunctionReturn(0);
}
*/

#undef __FUNCT__
#define __FUNCT__ "PetscMetadataAddDouble"
PetscErrorCode PetscMetadataAddDouble(const char *category, char *event, const char *name, double val) {
  int index;
  index = PMetadataObject.num;
  sprintf(PMetadataObject.PetscMetadataTable[index].category, category);
  sprintf(PMetadataObject.PetscMetadataTable[index].event, event);
  sprintf(PMetadataObject.PetscMetadataTable[index].name, name);
  sprintf(PMetadataObject.PetscMetadataTable[index].type, "double");
  sprintf(PMetadataObject.PetscMetadataTable[index].value,"%14.12e", val);
  PMetadataObject.num = index + 1;
}

#undef __FUNCT__  
#define __FUNCT__ "MatComputePropertiesVariance"
PetscErrorCode MatComputePropertiesVariance(Mat A, char *event) {
  PetscErrorCode ierr;
  PetscBool flg; 
  AnalysisItem res;
  int l;
  PetscFunctionBegin;
  
  ierr = RegisterVarianceModules(); CHKERRQ(ierr);

  ierr = ComputeQuantity(A,"variance","row-variability",(AnalysisItem*)&res,&l,&flg);
  if (flg) PetscMetadataAddDouble("MatrixProperty",event,"row-variability",res.r);

  ierr = ComputeQuantity(A,"variance","col-variability",(AnalysisItem*)&res,&l,&flg);
  if (flg) PetscMetadataAddDouble("MatrixProperty",event,"col-variability",res.r);

  ierr = ComputeQuantity(A,"variance","diagonal-variance",(AnalysisItem*)&res,&l,&flg);
  if (flg) PetscMetadataAddDouble("MatrixProperty",event,"diagonal-variance",res.r);

  ierr = ComputeQuantity(A,"variance","diagonal-average",(AnalysisItem*)&res,&l,&flg);
  if (flg) PetscMetadataAddDouble("MatrixProperty",event,"diagonal-average",res.r);

  ierr = ComputeQuantity(A,"variance","diagonal-sign",(AnalysisItem*)&res,&l,&flg);
  if (flg) PetscMetadataAddDouble("MatrixProperty",event,"diagonal-sign",res.r);

  PetscFunctionReturn(0);
}

#undef __FUNCT__  
#define __FUNCT__ "MatComputePropertiesAll"
/*@
   MatComputePropertiesAll - Computes matrix properties using Anamod.

   Collective on Mat

   Input Parameter:
.  A  - the matrix whose properties will be computed.

   Notes:
   Some properties may not be available for certain matrix types.

   Level: advanced

.keywords: Mat, context

.seealso: KSPGetOperators()
@*/
int MatComputePropertiesAll(Mat A) {
  PetscBool flg;
  int ncat,nmod,icat,imod; const char **cat,**mod;
  PetscErrorCode ierr;
  int indx;
  
  /*
   * Register the analysis modules
   */
  ierr = RegisterSimpleModules(); CHKERRQ(ierr);
  ierr = RegisterVarianceModules(); CHKERRQ(ierr);
  ierr = RegisterStructureModules(); CHKERRQ(ierr);
  ierr = RegisterSpectrumModules(); CHKERRQ(ierr);
  ierr = RegisterNormalityModules(); CHKERRQ(ierr);
  ierr = RegisterIprsModules(); CHKERRQ(ierr);

  indx = PMetadataObject.num;
  /*
   * Compute all available properties of the matrix
   */
  ierr = GetCategories(&ncat,&cat); CHKERRQ(ierr);
  for (icat=0; icat<ncat; icat++) {
    AnalysisDataType *types;
    ierr = CategoryGetModules
      (cat[icat],&mod,&types,PETSC_NULL,&nmod); CHKERRQ(ierr);
    for (imod=0; imod<nmod; imod++) {
      AnalysisDataType type; const char *string;
      ierr = GetDataType(cat[icat],mod[imod],&type, NULL); CHKERRQ(ierr);
      AnalysisItem result;
      switch (type) {
      case ANALYSISINTEGER : 
	{
	  ierr = ComputeQuantity
	    (A,cat[icat],mod[imod],&result,NULL,&flg); CHKERRQ(ierr);
	  if (flg) {
	    ierr = QuantityAsString(&result,type,&string); CHKERRQ(ierr);
	    printf("Computed <%s:%s> as <%s>\n",cat[icat],mod[imod],string);
            /* Update metadata */
            sprintf(PMetadataObject.PetscMetadataTable[indx].category, "MatrixProperties");
            sprintf(PMetadataObject.PetscMetadataTable[indx].event, event);
            sprintf(PMetadataObject.PetscMetadataTable[indx].name, mod[imod]);
            sprintf(PMetadataObject.PetscMetadataTable[indx].type, "int");
            sprintf(PMetadataObject.PetscMetadataTable[indx].value, string);
            indx++;
	    ierr = PetscFree(string); CHKERRQ(ierr);
	  } else printf("Anamod: Could not compute <%s:%s>\n",cat[icat],mod[imod]);
	}
	break;
      case ANALYSISDOUBLE :
	{
	  ierr = ComputeQuantity
	    (A,cat[icat],mod[imod],&result,NULL,&flg); CHKERRQ(ierr);
	  if (flg) {
	    ierr = QuantityAsString(&result,type,&string); CHKERRQ(ierr);
	    printf("Computed <%s:%s> as <%s>\n",cat[icat],mod[imod],string);
            /* Update metadata */
            sprintf(PMetadataObject.PetscMetadataTable[indx].category, "MatrixProperties");
            sprintf(PMetadataObject.PetscMetadataTable[indx].event, event);
            sprintf(PMetadataObject.PetscMetadataTable[indx].name, mod[imod]);
            sprintf(PMetadataObject.PetscMetadataTable[indx].type, "double");
            sprintf(PMetadataObject.PetscMetadataTable[indx].value, string);
            indx++;
	    ierr = PetscFree(string); CHKERRQ(ierr);
	  } else printf("Anamod: Could not compute <%s:%s>\n",cat[icat],mod[imod]);
	}
	break;
      /* BN: cannot handle array metadata yet (TODO)
      case ANALYSISINTARRAY :
	{
	  int *res;
	  ierr = ComputeQuantity
	    (A,cat[icat],mod[imod],(void*)&res,&flg); CHKERRQ(ierr);
	  if (flg) {
	    ierr = QuantityAsString((void*)&res,type,&string); CHKERRQ(ierr);
	    printf("Computed <%s:%s> as <%s>\n",cat[icat],mod[imod],string);
	    ierr = PetscFree(string); CHKERRQ(ierr);
	  } else printf("Could not compute <%s:%s>\n",cat[icat],mod[imod]);
	}
	break;
      case ANALYSISDBLARRAY :
	{
	  double *res;
	  ierr = ComputeQuantity
	    (A,cat[icat],mod[imod],(void*)&res,&flg); CHKERRQ(ierr);
	  if (flg) {
	    ierr = QuantityAsString((void*)&res,type,&string); CHKERRQ(ierr);
	    printf("Computed <%s:%s> as <%s>\n",cat[icat],mod[imod],string);
	    ierr = PetscFree(string); CHKERRQ(ierr);
	  } else printf("Could not compute <%s:%s>\n",cat[icat],mod[imod]);
	}
	break;
	*/
	/*
      case ANALYSISSTRING :
	{
	  char *res;
	  ierr = ComputeQuantity
	    (A,cat[icat],mod[imod],(void*)&res,&flg); CHKERRQ(ierr);
	  if (flg) {
	    ierr = QuantityAsString((void*)&res,type,&string); CHKERRQ(ierr);
	    printf("Computed <%s:%s> as <%s>\n",cat[icat],mod[imod],string);
	    ierr = PetscFree(string); CHKERRQ(ierr);
	  } else printf("Could not compute <%s:%s>\n",cat[icat],mod[imod]);
	}
	break;
	*/
      default : printf("Anamod: Can not handle type %d yet\n",type);
      }
    }
  }
  PMetadataObject.num=indx;

  PetscFunctionReturn(0);
}

int main() {
}
