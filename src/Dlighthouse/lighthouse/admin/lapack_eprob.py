from django.contrib import admin
from django import forms

from lighthouse.models.lapack_eprob import * 



class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
							    'cols':80,
							    'style':'font-family:monospace'}),
		help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')

	class Meta:
		app_label = 'lighthouse'
		#model = 'lapack_RoutineInfo'


""" Driver routines """
class lapack_eprob_simpleAdmin(admin.ModelAdmin):
	list_display = ('id','routineName', 'problem', 'complex', 'matrix', 'storage', 'schur', 'evaluerange', 'algorithm','balancing','schurform','queryPrecision')
	list_filter = ['problem', 'complex', 'matrix', 'storage', 'schur', 'evaluerange', 'algorithm','balancing','schurform','queryPrecision']
	search_fields = ['routineName',]
	ordering = ('info',)
	raw_id_fields = ('info',)

admin.site.register(lapack_eprob_simple, lapack_eprob_simpleAdmin)


