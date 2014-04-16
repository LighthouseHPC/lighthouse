from django.contrib import admin
from lighthouse.models.slepc_eprob import *


""" for slepc routine information """
class slepc_RoutineInfoAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']


admin.site.register(slepc_RoutineInfo, slepc_RoutineInfoAdmin)


class slepc_HermitianAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL','routineName')
	list_filter = [ 'routineName','spectrumType']
	search_fields = ['routineName', 'spectrumType']
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(slepc_Hermitian, slepc_HermitianAdmin)

class slepc_NonHermitianAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL','routineName')
	list_filter = [ 'routineName','spectrumType']
	search_fields = ['routineName', 'spectrumType']
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(slepc_NonHermitian, slepc_NonHermitianAdmin)

