#!/usr/bin/env python
'''#!/Users/lily/.virtualenvs/[lily]/lib/python2.7'''
import os
import sys

if __name__ == "__main__":
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "django_orthg.settings")

    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)
