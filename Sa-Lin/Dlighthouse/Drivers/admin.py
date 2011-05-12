from Drivers.models import Problem
from Drivers.models import RoutineInfo
from Drivers.models import LinearEquation
from Drivers.models import LinearLeastSquare
from Drivers.models import SymmetricEigenvalue
from Drivers.models import nonSymmetricEigenvalue
from django.contrib import admin
from django import forms



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
                                                            'cols':80,
                                                            'style':'font-family:monospace'}), 	
	       help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')
    
	class Meta:
        	model = RoutineInfo




class ProblemAdmin(admin.ModelAdmin):
	list_display = ('id', 'problem')
	ordering = ('id',)



class RoutineInfoAdmin(admin.ModelAdmin):
	list_display = ('id', 'routine')
	ordering = ('id',)
	search_fields = ['routine']
	form = EntryAdminForm



class LinearEquationAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)
	


class LinearLeastSquareAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)


class SymmetricEigenvalueAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)


class nonSymmetricEigenvalueAdmin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'structureType', 'url', 'description')
	
	list_filter = ['matrixType', 'thePrecision', 'structureType']
	search_fields = ['routineName', 'description']
	ordering = ('id',)
	raw_id_fields = ('problem', 'info',)




admin.site.register(Problem, ProblemAdmin)
admin.site.register(RoutineInfo, RoutineInfoAdmin)
admin.site.register(LinearEquation, LinearEquationAdmin)
admin.site.register(LinearLeastSquare, LinearLeastSquareAdmin)
admin.site.register(SymmetricEigenvalue, SymmetricEigenvalueAdmin)
admin.site.register(nonSymmetricEigenvalue, nonSymmetricEigenvalueAdmin)

