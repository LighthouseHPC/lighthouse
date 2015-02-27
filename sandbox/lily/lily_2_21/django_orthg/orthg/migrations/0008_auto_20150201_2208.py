# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('orthg', '0007_auto_20150201_2114'),
    ]

    operations = [
        migrations.RenameModel(
            old_name='orthg',
            new_name='lapack_least_guided',
        ),
    ]
