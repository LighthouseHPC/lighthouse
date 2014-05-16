#!/bin/bash

dj_root = "/Applications"

if [ "$(uname)" == "Linux" ]; then
    dj_root = "/opt" # django installed here by default
fi

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
sudo $pythonexec "whoosh==2.5.5"
echo "Installing Haystack"
sudo $pythonexec "django-haystack==2.1"
echo "Installing Dajaxice"
sudo $pythonexec django_dajaxice
echo "Installing Dajax"
sudo $pythonexec django_dajax
echo "Installing django_taggit"
sudo $pythonexec django_taggit
echo "Installing django-extensions"
sudo $pythonexec django-extensions

echo "(Please run manage.py syncdb to seed database if necessary.)"
echo "(Run src/Dlighthouse/lighthouse/libraries/lapack_le/databaseMng/databaseLoad.py if necessary.)"

# No longer needed for now (while using the sqlite3 db for demo purposes)
#echo ""; echo "--------------------------------------------------------"; 
#echo "Generating the database. If you get asked for superuser username and password creation, please create them and make sure to remember what they are."
#cd src/Dlighthouse &&  $pythonexec2 manage.py syncdb && cd ../..
