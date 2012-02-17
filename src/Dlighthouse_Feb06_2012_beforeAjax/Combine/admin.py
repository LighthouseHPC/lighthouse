from Driver.models import RoutineInfo
from Combine.models import LinearEquation_only
from django.contrib import admin
from django import forms



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
                                                            'cols':80,
                                                            'style':'font-family:monospace'}), 	
	       help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')
    
	class Meta:
        	model = RoutineInfo




class LinearEquation_onlyAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'url', 'notes')
	
	list_filter = ['matrixType', 'thePrecision', 'storageType']
	search_fields = ['routineName', 'notes']
	ordering = ('id',)
	raw_id_fields = ('info',)




admin.site.register(LinearEquation_only, LinearEquation_onlyAdmin)

