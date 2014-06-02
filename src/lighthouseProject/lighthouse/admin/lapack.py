from django.contrib import admin
from django import forms

from lighthouse.models.lapack_eigen import *
from lighthouse.models.lapack_svd import *
from lighthouse.models.lapack_sylvester import * 



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
							    'cols':80,
							    'style':'font-family:monospace'}),
		help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')

	class Meta:
		app_label = 'lighthouse'
		#model = 'lapack_RoutineInfo'


""" eigenproblem """
class lapack_eigen_Admin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'problem', 'standardGeneralized', 'matrixType', 'storageType',
			'selectedEV', 'eigenvector', 'schur', 'cndNumber')
	list_filter = ['problem', 'standardGeneralized', 'thePrecision', 'matrixType', 'storageType',]
	search_fields = ['routineName',]
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(lapack_eigen, lapack_eigen_Admin)





""" svd """
class lapack_svd_Admin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'problem', 'matrixType', 'storageType',
			'singularValues', 'singularVectors')
	list_filter = ['problem', 'thePrecision', 'matrixType', 'storageType',]
	search_fields = ['routineName',]
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(lapack_svd, lapack_svd_Admin)





""" Sylvester """
#class lapack_sylvester_Admin(admin.ModelAdmin):
#	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType')
#	list_filter = ['thePrecision', 'matrixType', 'storageType',]
#	search_fields = ['routineName',]
#	ordering = ('id',)
#	raw_id_fields = ('info',)
#
#admin.site.register(lapack_sylvester, lapack_sylvester_Admin)