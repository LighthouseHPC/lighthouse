from django.db import models


""" for routine information """
class lapack_RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', blank=True, null=True)

	def __unicode__(self):
		return self.info

	class Meta:
		app_label = 'lighthouse'
