#!/bin/bash

dj_root=~

pythonexec=easy_install
pythonexec2=python
dsversion=`ls $dj_root | grep djangostack | sed -e 's|djangostack-\(.*\)/|\1|'`
echo "Checking for python in $dj_root/$dsversion/python/bin"
if [ -e $dj_root/$dsversion/python/bin/easy_install ]; then 
  # Using djangostack executables
  pythonexec=$dj_root/$dsversion/python/bin/easy_install
  pythonexec2=$dj_root/$dsversion/python/bin/python
fi

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
echo "(Please run djangopythonpath/python manage.py syncdb to seed database if necessary.)"
echo " "
echo "(Run djangopythonpath/python databaseLoad.py in src/Dlighthouse/lighthouse/libraries/lapack_le/databaseMng/ if necessary.)"

# No longer needed for now (while using the sqlite3 db for demo purposes)
#echo ""; echo "--------------------------------------------------------"; 
#echo "Generating the database. If you get asked for superuser username and password creation, please create them and make sure to remember what they are."
#cd src/Dlighthouse &&  $pythonexec2 manage.py syncdb && cd ../..
