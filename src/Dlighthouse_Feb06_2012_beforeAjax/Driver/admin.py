from Driver.models import RoutineInfo
from Driver.models import LinearEquation_simple, LinearEquation_expert, LinearEquation_driver
from Driver.models import LinearLeastSquare
#from Driver.models import SymmetricEigenvalue
#from Driver.models import nonSymmetricEigenvalue
from Driver.models import Eigensolver
from django.contrib import admin
from django import forms



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
                                                            'cols':80,
                                                            'style':'font-family:monospace'}), 	
	       help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')
    
	class Meta:
        	model = RoutineInfo






class RoutineInfoAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']
	form = EntryAdminForm



class LinearEquation_simpleAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)
	



class LinearEquation_expertAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)




class LinearEquation_driverAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)




class LinearLeastSquareAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)



"""
class SymmetricEigenvalueAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)


class nonSymmetricEigenvalueAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)
"""



class EigensolverAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)





admin.site.register(RoutineInfo, RoutineInfoAdmin)
admin.site.register(LinearEquation_simple, LinearEquation_simpleAdmin)
admin.site.register(LinearEquation_expert, LinearEquation_expertAdmin)
admin.site.register(LinearEquation_driver, LinearEquation_driverAdmin)
admin.site.register(LinearLeastSquare, LinearLeastSquareAdmin)
#admin.site.register(SymmetricEigenvalue, SymmetricEigenvalueAdmin)
#admin.site.register(nonSymmetricEigenvalue, nonSymmetricEigenvalueAdmin)
admin.site.register(Eigensolver, EigensolverAdmin)

