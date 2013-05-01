from django.db import models


PRECISION_CHOICES = (
	(u's', u's(single)'), 
	(u'd', u'd(double)'), 
	(u'c', u'c(complex)'), 
	(u'z', u'z(complex double)'),
)

MATRIX_CHOICES = (
	(u'general', u'general'), 
	(u'symmetric', u'symmetric'), 
	(u'Hermitian', u'Hermitian'), 
	(u'SPD', u'SPD'),
	(u'HPD', u'HPD'),
	)

STORAGE_CHOICES = (
	(u'full', u'full'),
	(u'band', u'band'),
	(u'packed', u'packed'),
	(u'tridiagonal', u'tridiagonal'),
)  	




""" for routine information """
class lapack_RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', blank=True, null=True)

	def __unicode__(self):
		return self.info

	class Meta:
		app_label = 'lighthouse'








""" Driver routines """
class lapack_le_driver(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

	def __unicode__(self):
		return self.matrixType
		return self.storageType
	
	class Meta:
		app_label = 'lighthouse'




class lapack_le_simple(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

	def __unicode__(self):
		return self.matrixType
		return self.storageType

	class Meta:
		app_label = 'lighthouse'
		



class lapack_le_expert(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

	def __unicode__(self):
		return self.matrixType
		return self.storageType

	class Meta:
		app_label = 'lighthouse'








""" Computational routines """
class lapack_le_computational(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'
		



class lapack_le_factor(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'
		
		


class lapack_le_solve(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'




class lapack_le_condition_number(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'
		


class lapack_le_error_bound(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'




class lapack_le_invert(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'
		
		


class lapack_le_equilibrate(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)


        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'







""" Combine """
class lapack_le_only(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
        url = models.URLField()
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
                list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

        def __unicode__(self):
                return self.matrixType
                return self.storageType

	class Meta:
		app_label = 'lighthouse'
		
		
		
		
		


""" Arguments """
class lapack_le_arg(models.Model):
	routineName = models.CharField('Routine Name', max_length=20)
	param_in = models.CharField('In', max_length=40)
	param_out = models.CharField('Out', max_length=40)
	param_inout = models.CharField('In, Out', max_length=40)
	matrix = models.CharField('Matrix', max_length=10)
	array_1d_real = models.CharField('Array_1d_real', max_length=10)
	array_1d = models.CharField('Array_1d', max_length=20)
	array_1d_int = models.CharField('Array_1d_int', max_length=20)
	uplo = models.CharField('UPLO', max_length=10)
	integers = models.CharField('Integer', max_length=40)
	LDA_condition = models.CharField('LDA_condition', max_length=40)
	allocate_list = models.CharField('Allocate list', max_length=40)
	allocate = models.CharField('Allocate', max_length=100)
	readData = models.CharField('Read matrix', max_length=100)
	readData_L = models.CharField('Read matrix_LO', max_length=100)
	
	def __unicode__(self):
	    return self.routineName

	class Meta:
		app_label = 'lighthouse'