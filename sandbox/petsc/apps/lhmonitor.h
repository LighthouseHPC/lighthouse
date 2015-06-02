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

typedef struct {
    int currentSNESit;

} MonitorCtx;

#endif /* LHMONITOR_H_ */
