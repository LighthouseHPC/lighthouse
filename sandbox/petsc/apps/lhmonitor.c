/*
 * lhmonitor.c
 *
 *  Created on: May 31, 2015
 *      Author: norris
 */

#include "petsc.h"
#include "lhmonitor.h"

PetscErrorCode LHMonitor(SNES snes, PetscInt its, PetscReal norm, void*  monP) {
	MonitorCtx     *monP = (MonitorCtx*) ctx;
	KSP ksp;
	PC  pc;
	SNESGetKSP(snes,&ksp);
	KSPGetPC(ksp,&pc);
	//KSPView(ksp,PETSC_VIEWER_STDOUT_WORLD);
	//VecView(x,monP->viewer);
	return(0);
}


PetscErrorCode LHKSPMonitor(KSP snes, PetscInt its, PetscReal norm, void*  monP) {
	MonitorCtx     *monP = (MonitorCtx*) ctx;
	PC  pc;
	KSPGetPC(ksp,&pc);
	//KSPView(ksp,PETSC_VIEWER_STDOUT_WORLD);
	//VecView(x,monP->viewer);
	return(0);
}
