#!/bin/bash

pythonexec=easy_install
pythonexec2=python
dsversion=`ls /Applications | grep djangostack | sed -e 's|djangostack-\(.*\)/|\1|'`
echo "Checking for python in /Applications/$dsversion/python/bin"
if [ -e /Applications/$dsversion/python/bin/easy_install ]; then 
  # Using djangostack
  pythonexec=/Applications/$dsversion/python/bin/easy_install
  pythonexec2=/Applications/$dsversion/python/bin/python
fi

echo "Using python $pythonexec2"

echo "Installing Whoosh"
sudo $pythonexec whoosh
echo "Installing Haystack"
sudo $pythonexec django-haystack
echo "Installing Dajaxice"
sudo $pythonexec django_dajaxice
echo "Installing Dajax"
sudo $pythonexec django_dajax

# No longer needed for now (while using the sqlite3 db for demo purposes)
#echo ""; echo "--------------------------------------------------------"; 
#echo "Generating the database. If you get asked for superuser username and password creation, please create them and make sure to remember what they are."
#cd src/Dlighthouse &&  $pythonexec2 manage.py syncdb && cd ../..
