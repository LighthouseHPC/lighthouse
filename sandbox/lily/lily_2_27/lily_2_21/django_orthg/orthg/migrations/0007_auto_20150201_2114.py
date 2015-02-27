# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('orthg', '0006_auto_20150123_0548'),
    ]

    operations = [
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
        migrations.AddField(
            model_name='orthg',
            name='info',
            field=models.ForeignKey(default='', to='orthg.lapack_RoutineInfo'),
            preserve_default=False,
        ),
    ]
