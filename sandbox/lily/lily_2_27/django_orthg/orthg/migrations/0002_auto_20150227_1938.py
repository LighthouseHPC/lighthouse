# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('orthg', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='current_question',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('question', models.CharField(max_length=200, verbose_name=b'current question')),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.CreateModel(
            name='lapack_least_guided',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('thePrecision', models.CharField(max_length=10, verbose_name=b'precision', choices=[('s', 'single'), ('d', 'double'), ('c', 'complex'), ('z', 'complex double')])),
                ('routineName', models.CharField(max_length=30, verbose_name=b'routine name')),
                ('standardGeneralized', models.CharField(max_length=20, verbose_name=b'standard/generalized', choices=[('standard', 'standard'), ('generalized', 'generalized')])),
                ('complexNumber', models.CharField(max_length=10, verbose_name=b'complex number', choices=[('no', 'no'), ('yes', 'yes')])),
                ('FullStorage', models.CharField(max_length=60, verbose_name=b'storage and full', choices=[('full', 'full'), ('band', 'band'), ('packed', 'packed'), ('tridiagonal', 'tridiagonal'), ('bidiagonal', 'bidiagonal'), ('RFP', 'RFP'), ('full/packed/band/tridiagonal', 'full/packed/band/tridiagonal'), ('bidiagonal/band', 'bidiagonal/band')])),
                ('sFullRank', models.CharField(max_length=225, verbose_name=b'sFullRank', choices=[('no', 'no'), ('yes', 'yes')])),
                ('gFullRank', models.CharField(max_length=225, verbose_name=b'gFullRank', choices=[('Neither are Full Rank', 'Neither are Full Rank'), ('Both are Full Rank', 'Both are Full Rank'), ('Only A is Full Rank', 'Only A is Full Rank'), ('Only B is Full Rank', 'Only B is Full Rank')])),
                ('svd', models.CharField(max_length=225, verbose_name=b'svd', choices=[('SVD', 'SVD'), ('divide and conquer', 'divide and conquer')])),
                ('qr', models.CharField(max_length=225, verbose_name=b'qr', choices=[('QR Column pivoting', 'QR Column pivoting'), ('Faster QR', 'Faster QR')])),
                ('singleDouble', models.CharField(max_length=10, verbose_name=b'single/double', choices=[('single', 'single'), ('double', 'double')])),
                ('notes', models.CharField(max_length=225, verbose_name=b'notes')),
                ('current_question', models.ForeignKey(to='orthg.current_question')),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.CreateModel(
            name='lapack_RoutineInfo',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('routine', models.CharField(max_length=30, verbose_name=b'Routine')),
                ('info', models.TextField(default=b'', null=True, verbose_name=b'Information', blank=True)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.RemoveField(
            model_name='choice',
            name='question',
        ),
        migrations.DeleteModel(
            name='Choice',
        ),
        migrations.DeleteModel(
            name='Question',
        ),
        migrations.AddField(
            model_name='lapack_least_guided',
            name='info',
            field=models.ForeignKey(to='orthg.lapack_RoutineInfo'),
            preserve_default=True,
        ),
    ]
