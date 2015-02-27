# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('orthg', '0005_auto_20150121_1721'),
    ]

    operations = [
        migrations.AlterField(
            model_name='orthg',
            name='gFullRank',
            field=models.CharField(max_length=225, verbose_name=b'gFullRank', choices=[('Neither are Full Rank', 'Neither are Full Rank'), ('Both are Full Rank', 'Both are Full Rank'), ('Only A is Full Rank', 'Only A is Full Rank'), ('Only B is Full Rank', 'Only B is Full Rank')]),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='orthg',
            name='qr',
            field=models.CharField(max_length=225, verbose_name=b'qr', choices=[('QR Column pivoting', 'QR Column pivoting'), ('Faster QR', 'Faster QR')]),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='orthg',
            name='sFullRank',
            field=models.CharField(max_length=225, verbose_name=b'sFullRank', choices=[('no', 'no'), ('yes', 'yes')]),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='orthg',
            name='svd',
            field=models.CharField(max_length=225, verbose_name=b'svd', choices=[('SVD', 'SVD'), ('divide and conquer', 'divide and conquer')]),
            preserve_default=True,
        ),
    ]
