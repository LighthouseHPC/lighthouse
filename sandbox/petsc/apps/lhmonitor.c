/*
 * lhmonitor.c
 *
 *  Created on: May 31, 2015
 *      Author: norris
 */

#include "petsc.h"
#include "lhmonitor.h"

PetscErrorCode LHMonitorInit(MonitorCtx *monP) {
	monP->lastSNESit = 0;
	monP->matrixCount = 0;
	return(0);
}

PetscErrorCode LHMonitor(SNES snes, PetscInt its, PetscScalar norm, void*  ctx) {
	MonitorCtx *monP = (MonitorCtx*) ctx;
	monP->currentSNESit = its;
	/*
	KSP ksp;
	PC  pc;
	SNESGetKSP(snes,&ksp);
	KSPGetPC(ksp,&pc);
	KSPView(ksp,PETSC_VIEWER_STDOUT_WORLD);
	VecView(x,monP->viewer);
	*/
	return(0);
}


PetscErrorCode LHKSPMonitor(KSP ksp, PetscInt its, PetscScalar norm, void*  ctx) {
	MonitorCtx     *monP = (MonitorCtx*) ctx;
	Mat A;
	PetscInt ierr;
	PetscViewer view;

	if (its > 0 && monP->currentSNESit > 0 && monP->matrixCount < MAX_NUM_MATRICES && monP->currentSNESit != monP->lastSNESit) {
		ierr = KSPGetOperators(ksp, &A, NULL); CHKERRQ(ierr);
		char fname[128];
		sprintf(fname,"%s_%d.petsc", monP->matrixName, monP->matrixCount);
		ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,fname,FILE_MODE_WRITE,&view); CHKERRQ(ierr);
		/* Matview will write the Mat object to binary file */
		ierr = MatView(A,view); CHKERRQ(ierr);
		ierr = PetscViewerDestroy(&view); CHKERRQ(ierr);
		(monP->matrixCount)++;
		monP->lastSNESit = monP->currentSNESit;
	}
	//KSPView(ksp,PETSC_VIEWER_STDOUT_WORLD);
	//VecView(x,monP->viewer);
	//PetscPrintf(PETSC_COMM_WORLD,"Hi from LHKSPMonitor: %d %d %g\n", monP->currentSNESit, its, norm);
	return(0);
}
