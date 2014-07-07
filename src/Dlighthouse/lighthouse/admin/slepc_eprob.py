from django.contrib import admin
from lighthouse.models.slepc_eprob import *


""" for slepc routine information """
class slepc_RoutineInfoAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']


admin.site.register(slepc_RoutineInfo, slepc_RoutineInfoAdmin)


class slepc_treeLeftAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'probType', 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL', 'isBinary', 'perfIndex', 'routineName', 'info')
	list_filter = [ 'routineName','spectrumType', 'probType', 'isBinary', 'isComplex']
	search_fields = ['routineName', 'spectrumType']
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(slepc_treeLeft, slepc_treeLeftAdmin)

class slepc_treeRightAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'probType', 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL', 'isBinary', 'perfIndex', 'routineName', 'info')
	list_filter = [ 'routineName','spectrumType', 'probType', 'isBinary', 'isComplex']
	search_fields = ['routineName', 'spectrumType']
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(slepc_treeRight, slepc_treeRightAdmin)

