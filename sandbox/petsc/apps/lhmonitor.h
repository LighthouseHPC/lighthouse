/*
 * lhmonitor.h
 *
 *  Created on: May 31, 2015
 *      Author: norris
 */

#ifndef LHMONITOR_H_
#define LHMONITOR_H_

PetscErrorCode LHMonitor(SNES,PetscInt,PetscReal,void*);
PetscErrorCode LHKSPMonitor(KSP,PetscInt,PetscReal,void*);

#define MAX_NUM_MATRICES 10

typedef struct {
    int currentSNESit;
    int lastSNESit;
    int matrixCount;
    char matrixName[50];
} MonitorCtx;

PetscErrorCode LHMonitorInit(MonitorCtx *monP);

#endif /* LHMONITOR_H_ */
