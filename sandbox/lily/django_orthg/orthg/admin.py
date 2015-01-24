from django.contrib import admin
from django import forms
from orthg.models import *

class EntryAdminForm(forms.ModelForm):
	info = forms.CharField(widget=forms.Textarea(attrs={'rows':50,
							    'cols':80,
							    'style':'font-family:monospace'}),
		help_text='<a href="http://www.netlib.org/lapack/" target="_blank">LAPACK Official Site</a>')

	class Meta:
		app_label = 'lighthouse'
		#model = 'lapack_RoutineInfo'


class orthg_Admin(admin.ModelAdmin):
#add: routineName
	list_display = ('id', 'thePrecision', 'standardGeneralized', 'sFullRank', 'gFullRank', 'svd', 'qr', )
	list_filter = ['standardGeneralized', 'thePrecision', 'sFullRank', 'gFullRank', 'svd', 'qr',]
	search_fields = ['routineName',]
	ordering = ('id',)
	#raw_id_fields = ('info',)

admin.site.register(orthg, orthg_Admin)



