# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('orthg', '0003_auto_20150118_2353'),
    ]

    operations = [
        migrations.CreateModel(
            name='orthg',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('thePrecision', models.CharField(max_length=10, verbose_name=b'precision', choices=[('s', 'single'), ('d', 'double'), ('c', 'complex'), ('z', 'complex double')])),
                ('routineName', models.CharField(max_length=30, verbose_name=b'routine name')),
                ('standardGeneralized', models.CharField(max_length=20, verbose_name=b'standard/generalized', choices=[('standard', 'standard'), ('generalized', 'generalized')])),
                ('complexNumber', models.CharField(max_length=10, verbose_name=b'complex number', choices=[('no', 'no'), ('yes', 'yes')])),
                ('storageFull', models.CharField(max_length=60, verbose_name=b'storage and full', choices=[('full', 'full'), ('band', 'band'), ('packed', 'packed'), ('tridiagonal', 'tridiagonal'), ('bidiagonal', 'bidiagonal'), ('RFP', 'RFP'), ('full/packed/band/tridiagonal', 'full/packed/band/tridiagonal'), ('bidiagonal/band', 'bidiagonal/band')])),
                ('sFullRank', models.CharField(max_length=225, verbose_name=b'sFullRank')),
                ('gFullRank', models.CharField(max_length=225, verbose_name=b'gFullRank')),
                ('svd', models.CharField(max_length=225, verbose_name=b'svd')),
                ('qr', models.CharField(max_length=225, verbose_name=b'qr')),
                ('singleDouble', models.CharField(max_length=10, verbose_name=b'single/double', choices=[('single', 'single'), ('double', 'double')])),
                ('notes', models.CharField(max_length=225, verbose_name=b'notes')),
            ],
            options={
            },
            bases=(models.Model,),
        ),
    ]
