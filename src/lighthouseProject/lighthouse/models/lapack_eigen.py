from django.db import models
from lighthouse.models.lapack_choiceDict import *
from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo



###---------------- for guided search ----------------###
class lapack_eigen_guided(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        problem = models.CharField('problem type', max_length=50, choices=EIGENPROBLEM_CHOICES)
        standardGeneralized = models.CharField('standard/generalized', max_length=20, choices=STANDARD_CHOICES)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('storage type', max_length=30, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('selected eigenvalues', max_length=10, choices=NOYESNONE_CHOICES)
        eigenvector = models.CharField('eigenvectors', max_length=10, choices=NOYESBOTH_CHOICES)
        schur = models.CharField('Schur form/vectors', max_length=30, choices=NOYESNONE_CHOICES)
        cndNumber = models.CharField('condition number', max_length=10, choices=NOYESNONE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'lighthouse'
                
                
                

###---------------- for advanced search ----------------###
class lapack_eigen_driver_standard_sh(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('storage type', max_length=30, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('selected eigenvalues', max_length=10, choices=NOYESNONE_CHOICES)
        method = models.CharField('method', max_length=50, choices=METHOD_dssh_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'lighthouse'
                

class lapack_eigen_driver_standard_g(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        function = models.CharField('function', max_length=150, choices=FUNCTION_dsg_CHOICES)
        method = models.CharField('method', max_length=50)
        cndNumber = models.CharField('condition number', max_length=10, choices=NOYESNONE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'lighthouse'
                
                
class lapack_eigen_driver_generalized_sh(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('storage type', max_length=30, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('selected eigenvalues', max_length=10, choices=NOYESNONE_CHOICES)
        method = models.CharField('method', max_length=50, choices=METHOD_dgsh_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'lighthouse'
         
                
class lapack_eigen_driver_generalized_g(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        function = models.CharField('function', max_length=150, choices=FUNCTION_dgg_CHOICES)
        method = models.CharField('method', max_length=50)
        cndNumber = models.CharField('condition number', max_length=10, choices=NOYES_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'lighthouse'
                
                
class lapack_eigen_computational_standard_sh(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        function = models.CharField('function', max_length=150, choices=FUNCTION_cssh_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('storage type', max_length=30, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('selected eigenvalues', max_length=10, choices=NOYESNONE_CHOICES)
        eigenvector = models.CharField('eigenvectors', max_length=10, choices=NOYESNONE_CHOICES)
        method = models.CharField('method', max_length=50, choices=METHOD_cssh_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=200)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                       
        class Meta:
                app_label = 'lighthouse'
                
                
class lapack_eigen_computational_standard_g(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        function = models.CharField('function', max_length=150, choices=FUNCTION_csg_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        method = models.CharField('method', max_length=50, choices=METHOD_csg_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=200)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                       
        class Meta:
                app_label = 'lighthouse'
                
                
                
class lapack_eigen_computational_generalized_sh(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        function = models.CharField('function', max_length=150, choices=FUNCTION_cgsh_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('storage type', max_length=30, choices=STORAGE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=200)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                       
        class Meta:
                app_label = 'lighthouse'
                
                
                
class lapack_eigen_computational_generalized_g(models.Model):
        thePrecision = models.CharField('precision', max_length=3, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        function = models.CharField('function', max_length=150, choices=FUNCTION_cgg_CHOICES)
        storageType = models.CharField('storage type', max_length=30, choices=STORAGE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        purpose = models.CharField('purpose', max_length=200)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                       
        class Meta:
                app_label = 'lighthouse'
                