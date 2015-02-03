#!/bin/bash

# Make sure executed from djangostack
if [[ $PATH != *djangostack* ]]
then
  echo "ERROR: Please run ./use_djangostack from your installation of"
  echo "       djangostack in this shell before running the bootstrap!"
  echo "Exiting."
  exit 1  
fi

pythonexec=easy_install
pythonexec2=python

echo "Using python $pythonexec2"

echo "Installing Whoosh"
$pythonexec "whoosh==2.5.5"
echo "Installing Haystack"
$pythonexec "django-haystack==2.1"
echo "Installing Dajaxice"
$pythonexec django_dajaxice
echo "Installing Dajax"
$pythonexec django_dajax
echo "Installing django_taggit"
$pythonexec django_taggit
echo "Installing django-extensions"
$pythonexec django-extensions

echo " "
echo "(Please run 'python manage.py syncdb' from src/Dlighthouse to seed database as necessary.)"
echo " "
echo "(Then run 'python databaseLoad.py' from src/Dlighthouse/lighthouse/database/lapack_le/databaseMng/ as necessary.)"

