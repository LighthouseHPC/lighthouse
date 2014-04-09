from django.contrib import admin
from django import forms

from lighthouse.models.lapack_eigen import * 



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
							    'cols':80,
							    'style':'font-family:monospace'}),
		help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')

	class Meta:
		app_label = 'lighthouse'
		#model = 'lapack_RoutineInfo'


""" Driver routines """
class lapack_eigen_standard_Admin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'complexNumber', 'matrixType', 'storageType',
			'selectedEV', 'eigenvector', 'eigenvector_schur', 'cndNumber', 'notes','info')
	list_filter = ['complexNumber', 'matrixType', 'storageType',]
	search_fields = ['routineName',]
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(lapack_eigen_standard, lapack_eigen_standard_Admin)


