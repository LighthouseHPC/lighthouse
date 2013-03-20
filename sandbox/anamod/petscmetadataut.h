#if !defined(__METADATA_H)
#define __METADATA_H

#include "petscsnes.h"

#define MAX_STRING 40
#define METADATA_TABLE_SIZE 10000

typedef struct PetscMetaDataAttribute {
  char category[MAX_STRING];
  char event[MAX_STRING];
  char name[MAX_STRING]; 
  char type[MAX_STRING]; 
  char value[MAX_STRING];
  } PETSCMETADATAATTRIBUTE;

typedef struct PetscMetadataObject{ 
  PETSCMETADATAATTRIBUTE 
  PetscMetadataTable[METADATA_TABLE_SIZE]; 
  int num;
  } PETSCMETADATAOBJECT;

PetscErrorCode PetscMetadataInit(void);

PetscErrorCode MetadataSetEvent(char e[]);

PetscErrorCode SNESMonitorMetadata(SNES,PetscInt,PetscReal,void *);

PetscErrorCode AppMetadataInit(void *);

PetscErrorCode AppMonitorMetadata(SNES, PetscInt, PetscReal, void *);

PetscErrorCode AppMetadataFini(void);

PetscErrorCode PetscMetadataFini(void);

PetscErrorCode MatComputePropertiesVariance(Mat, char*);

#endif
