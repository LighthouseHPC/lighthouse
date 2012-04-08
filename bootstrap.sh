#!/bin/bash 

pythonexec=easy_install

if [ -e /Applications/djangostack-1.3.1-3/python/bin/easy_install ]; then 
  # Using djangostack
  pythonexec=/Applications/djangostack-1.3.1-3/python/bin/easy_install
  pythonexec2=/Applications/djangostack-1.3.1-3/python/bin/python
fi


echo "Installing Whoosh"
sudo $pythonexec whoosh
echo "Installing Haystack"
sudo $pythonexec django-haystack


# No longer needed for now (while using the sqlite3 db for demo purposes)
#echo ""; echo "--------------------------------------------------------"; 
#echo "Generating the database. If you get asked for superuser username and password creation, please create them and make sure to remember what they are."
#cd src/Dlighthouse &&  $pythonexec2 manage.py syncdb && cd ../..
