from Drivers.models import RoutineInfo
from Combine.models import LinearEquation_comb, LinearEquation_trans
from django.contrib import admin
from django import forms



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
                                                            'cols':80,
                                                            'style':'font-family:monospace'}), 	
	       help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')
    
	class Meta:
        	model = RoutineInfo






class LinearEquation_combAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)




class LinearEquation_transAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)





admin.site.register(LinearEquation_comb, LinearEquation_combAdmin)
admin.site.register(LinearEquation_trans, LinearEquation_transAdmin)

