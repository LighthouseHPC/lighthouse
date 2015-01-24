# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('orthg', '0004_orthg'),
    ]

    operations = [
        migrations.RenameField(
            model_name='orthg',
            old_name='storageFull',
            new_name='FullStorage',
        ),
    ]
