from django.contrib import admin
from django import forms

from lapack.models import RoutineInfo
from lapack.models import LinearEquation_simple, LinearEquation_expert, LinearEquation_driver
from lapack.models import LinearEquation_computational, LinearEquation_factor, LinearEquation_solve, LinearEquation_condition_number, LinearEquation_error_bound, LinearEquation_invert, LinearEquation_equilibrate
from lapack.models import LinearEquation_only



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
                                                            'cols':80,
                                                            'style':'font-family:monospace'}), 	
	       help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')
    
	class Meta:
        	model = RoutineInfo





""" for routine information """
class RoutineInfoAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']
	form = EntryAdminForm





""" Driver routines """
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


admin.site.register(RoutineInfo, RoutineInfoAdmin)
admin.site.register(LinearEquation_simple, LinearEquation_simpleAdmin)
admin.site.register(LinearEquation_expert, LinearEquation_expertAdmin)
admin.site.register(LinearEquation_driver, LinearEquation_driverAdmin)





""" Computational routines """
class LinearEquation_computationalAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)





class LinearEquation_factorAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class LinearEquation_solveAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class LinearEquation_condition_numberAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class LinearEquation_error_boundAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class LinearEquation_invertAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class LinearEquation_equilibrateAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)



admin.site.register(LinearEquation_computational, LinearEquation_computationalAdmin)
admin.site.register(LinearEquation_factor, LinearEquation_factorAdmin)
admin.site.register(LinearEquation_solve, LinearEquation_solveAdmin)
admin.site.register(LinearEquation_condition_number, LinearEquation_condition_numberAdmin)
admin.site.register(LinearEquation_error_bound, LinearEquation_error_boundAdmin)
admin.site.register(LinearEquation_invert, LinearEquation_invertAdmin)
admin.site.register(LinearEquation_equilibrate, LinearEquation_equilibrateAdmin)






""" Combine """
class LinearEquation_onlyAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




admin.site.register(LinearEquation_only, LinearEquation_onlyAdmin)

