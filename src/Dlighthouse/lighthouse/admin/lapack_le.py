from django.contrib import admin
from django import forms

from lighthouse.models.lapack_le import * 



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
							    'cols':80,
							    'style':'font-family:monospace'}),
		help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')

	class Meta:
		app_label = 'lighthouse'
		#model = 'lapack_RoutineInfo'





""" for lapack routine information """
class lapack_RoutineInfoAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']
	form = EntryAdminForm



admin.site.register(lapack_RoutineInfo, lapack_RoutineInfoAdmin)







""" Driver routines """
class lapack_le_simpleAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)
	



class lapack_le_expertAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)




class lapack_le_driverAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)





admin.site.register(lapack_le_simple, lapack_le_simpleAdmin)
admin.site.register(lapack_le_expert, lapack_le_expertAdmin)
admin.site.register(lapack_le_driver, lapack_le_driverAdmin)







""" Computational routines """
class lapack_le_computationalAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)





class lapack_le_factorAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class lapack_le_solveAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class lapack_le_condition_numberAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class lapack_le_error_boundAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class lapack_le_inverseAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)




class lapack_le_equilibrateAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)



admin.site.register(lapack_le_computational, lapack_le_computationalAdmin)
admin.site.register(lapack_le_factor, lapack_le_factorAdmin)
admin.site.register(lapack_le_solve, lapack_le_solveAdmin)
admin.site.register(lapack_le_condition_number, lapack_le_condition_numberAdmin)
admin.site.register(lapack_le_error_bound, lapack_le_error_boundAdmin)
admin.site.register(lapack_le_inverse, lapack_le_inverseAdmin)
admin.site.register(lapack_le_equilibrate, lapack_le_equilibrateAdmin)






""" Combine """
class lapack_le_onlyAdmin(admin.ModelAdmin):
        list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
        
        list_filter = ['matrixType', 'thePrecision', 'storageType']
        search_fields = ['routineName', 'notes']
        ordering = ('id',)
        raw_id_fields = ('info',)


admin.site.register(lapack_le_only, lapack_le_onlyAdmin)






""" Arguments """
class lapack_le_argAdmin(admin.ModelAdmin):
	list_display = ('id', 'routineName', 'param_all', 'param_in', 'param_out', 'param_inout',
			'matrix', 'array_1d_real', 'array_1d', 'array_1d_int', 'char', 'integers', 'reals',
			'LDA_condition', 'allocate_list', 'allocate', 'readData', 'readData_L', 'other')
	
	#list_filter = ['matrix']
	search_fileds = ['routineName']
        ordering = ('id',)
	
	
admin.site.register(lapack_le_arg, lapack_le_argAdmin)



class lapack_le_arg_cAdmin(admin.ModelAdmin):
	list_display = ('id', 'routineName', 'param', 'define', 'char', 'global_var', 'integers', 'array_real',
			'array_complex', 'other')
	
	#list_filter = ['matrix']
	search_fileds = ['routineName']
        ordering = ('id',)
	
	
admin.site.register(lapack_le_arg_c, lapack_le_arg_cAdmin)

