from django.db import models
from lighthouse.models.lapack_choiceDict import *
from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo



###---------------- for guided search ----------------###
###--- Eigenproblem ---###
class orthg_question(models.Model):
        thePrecision = models.CharField('precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        problem = models.CharField('problem_type', max_length=50, choices=EIGENPROBLEM_CHOICES)
        complexNumber = models.CharField('complex', max_length=10, choices=NOYES_CHOICES)
        rank = models.CharField('rank', max_length=30, choices=MATRIX_CHOICES)
        storage = models.CharField('storage', max_length=60, choices=STORAGE_CHOICES)
        col_pivot = models.CharField('col_pivot', max_length=10, choices=NOYESNONE_CHOICES)
        svd = models.CharField('svd', max_length=30, choices=NOYESNONE_CHOICES)
        A_B_blank = models.CharField('A_B_blank', max_length=10, choices=NOYESNONE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

class orthg_choice(models.Model):
    question = models.ForeignKey(orthg_question)
    choice_text = models.CharField(max_length=200)
    votes = models.IntegerField(default=0)                
                
                
