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


""" standard eigenproblems """
class lapack_eigen_standard_Admin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType',
			'selectedEV', 'eigenvector', 'schur', 'cndNumber')
	list_filter = ['thePrecision', 'matrixType', 'storageType',]
	search_fields = ['routineName',]
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(lapack_eigen_standard, lapack_eigen_standard_Admin)



""" generalized eigenproblems """
class lapack_eigen_generalized_Admin(admin.ModelAdmin):
	list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType',
			'selectedEV', 'eigenvector', 'schur', 'cndNumber')
	list_filter = ['thePrecision', 'matrixType', 'storageType',]
	search_fields = ['routineName',]
	ordering = ('id',)
	raw_id_fields = ('info',)

admin.site.register(lapack_eigen_generalized, lapack_eigen_generalized_Admin)