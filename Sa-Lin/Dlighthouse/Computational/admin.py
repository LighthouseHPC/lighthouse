from Computational.models import Problem
from Computational.models import RoutineInfo_Comput
from Computational.models import Eigensolver_Comput
from django.contrib import admin
from django import forms



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
                                                            'cols':80,
                                                            'style':'font-family:monospace'}), 	
	       help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')
    
	class Meta:
        	model = RoutineInfo_Comput




class ProblemAdmin(admin.ModelAdmin):
	list_display = ('id', 'problem')
	ordering = ('id',)



class RoutineInfo_ComputAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']
	form = EntryAdminForm




class Eigensolver_ComputAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)




admin.site.register(Problem, ProblemAdmin)
admin.site.register(RoutineInfo_Comput, RoutineInfo_ComputAdmin)
admin.site.register(Eigensolver_Comput, Eigensolver_ComputAdmin)

