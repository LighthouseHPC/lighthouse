# Lighthouse
Lighthouse is a framework for creating, maintaining, and using a taxonomy of available software that can be used to build highly-optimized matrix algebra computations. The taxonomy provides an organized anthology of software components and programming tools needed for that task. The taxonomy will serve as a guide to practitioners seeking to learn what is available for their programming tasks, how to use it, and how the various parts fit together. It builds upon and improves existing collections of numerical software, adding tools for the tuning of matrix algebra computations.

The development version of Lighthouse can be accessed [here](http://lighthouse.cs.uoregon.edu). If you would like to check out the code and run it on Linux or Mac OS X 10.6 or later, refer to the [Getting Started] page.

Acknowledgment: This material is based upon work supported by the National Science Foundation under Grants No. 0916474 and 1219089.

### Version
0.5.0

### Publications

* [Lighthouse: A User-Centered Web Service for Linear Algebra Software](http://arxiv.org/abs/1408.1363)
Boyana Norris, Sa-Lin Bernstein, Ramya Nair, Elizabeth Jessup
Elsevier Journal of Systems and Software (JSS): Special Issue on Software Engineering for Parallel Systems, (2014)

* [Generating Customized Sparse Eigenvalue Solutions with Lighthouse](http://hpcl.cs.uoregon.edu/pubs/iccgi14.pdf)
Ramya Nair, Sa-Lin Bernstein, Elizabeth Jessup, Boyana Norris
Proceedings of the Ninth International Multi-Conference on Computing in the Global Information Technology June 22 - 17, 2014, Seville, Spain, (2014)

### Getting Started

The goal of the Lighthouse framework is to assist
scientists, engineers, and students with the implementation of the matrix algebra 
computations that dominate many high-performance applications. Like a lighthouse 
that illuminates a dark sea and guides ships, Lighthouse will guide the practitioners 
through the stormy seas of numerical software development. 
Lighthouse was inspired by the [LAPACK Search Engine](http://www.cs.colorado.edu/~lapack) and 
two usability studies of later search prototypes. 
Lighthouse is the first framework that attempts to combine 
a matrix algebra software ontology with code generation and tuning capabilities. It 
contains a taxonomy that will provide all of the software needed to take a matrix 
algebra problem from algorithm description to a high-performance implementation. 
Moreover, Lighthouse offers different levels of interfaces to assist users of different 
backgrounds to exploit the numerical software and the different code generation and 
tuning tools included in the taxonomy. 
#### Trying a demo version of our server 

If you are running Mac OS X 10.6 or later, or Linux, you can get the Lighthouse server running locally with relatively little effort. Note that because we make the database setup as trivial as possible, this will run _much_ slower than production servers based on mysql do.

#### Installing and running your own Lighthouse server
##### 1. Download and install [DjangoStack](http://bitnami.org/stack/djangostack), *version 1.4.12*.
  * On Linux, djangostack suggests installing in `/opt` if you execute the installer as root. For our purposes, you should execute the installer as a normal user and install to your home directory, as root privileges will complicate things later on.
  * For database choices, only install MySQL.
  * If there is an error related to using MySQL (port in use, for example), you may change the port number. This change should eventually be reflected in our project source tree at `src/Dlighthouse/settings.py`
  * When you set up database passwords for djangostack, use `yellow1234`.


The following steps all take place on the command line of a Terminal window.

##### 2. Use djangostack.

Djangostack is a compatible set of server, database, and scripting applications (mysql, apache, python, etc) that are installed and used  separately from any existing versions on your host system. *It is imperative these are used when you work on the command line with Lighthouse software. Before setting up Lighthouse according to the steps below -- and before running it in typical usage -- you must run the following two commands*:

```
user@localhost:~/djangostack-1.4-xx $ ./use_djangostack
```

This script ensures that djangostack's executable paths to python _et al_. are selected in your shell environment, and launches a new `bash` shell under that environment.

##### 3. Starting the server
The above command doesn't actually start the services provided by djangostack. For that, run:
```
user@localhost:~/djangostack-1.4-xx $ ./ctlscript.sh start
```
(`./ctlscript.sh help` for more information.) The script starts the services in the background and returns you to the shell when ready.

##### 4. Set up mysql database
You will need to create the mysql database and a user for it before installing Lighthouse. After starting services as described above, run mysql using the 'root' user and password assigned during djangostack installation (yellow1234):
```
$ mysql -u root -p
Enter password:
...
mysql> create database lighthousedb;
```
Next, create the appropriate user name (lighthouse) for accessing the DB (lighthousedb), assign permissions, and exit:
```
mysql> CREATE USER 'lighthouse'@'localhost' IDENTIFIED BY 'yellow1234';
mysql> GRANT ALL ON *.* TO 'lighthouse'@'localhost';
mysql> exit;
```

##### 5. Restart server
Restart djangostack services in order to get MySQL changes working. `cd` to the djangostack directory and run
```
./ctlscript.sh restart
```

##### 6. Get the Lighthouse sources
Clone or download [Lighthouse](https://github.com/LighthouseHPC/lighthouse). First, `cd` to your preferred directory for storing the lighthouse source tree.

```
git clone https://github.com/LighthouseHPC/lighthouse.git 
```
Then change to it:
```
cd lighthouse
```

##### 7. Bootstrap 
Bootstrap required python modules and update the database (only once, no need to do again)

*On Mac*:
```
./scripts/bootstrap.sh
```
*On Linux*:
```
./scripts/bootstrap-new.sh
```
This may ask you for your superuser password on the Mac because it's adding packages to the djangostack python installation (root password should not be required on Linux since it is installed under your home directory). If bootstrap doesn't work, you can install the required packages manually, e.g., 

```
easy_install whoosh==2.5.5
easy_install django-haystack==2.1
easy_install django_dajaxice
easy_install django_dajax
easy_install django_taggit
easy_install django-extensions
```

You will then see information about seeding the database when the bootstrap script finishes.

Seed the DB by running from src/Dlighthouse (remember to be under the djangostack environment by running use_djangostack as described above)
```
user@localhost:~/lighthouse/src/Dlighthouse$ python manage.py syncdb
```
Change to `src/Dlighthouse/lighthouse/database/lapack_le/databaseMng` and run
```
user@localhost:~/lighthouse/src/Dlighthouse/lighthouse/database/lapack_le/databaseMng$ python databaseLoad.py
```

You will need to do the same for each appropriate directory under
`lighthouse/src/Dlighthouse/lighthouse/database/`, i.e., `lapack_svd`, `slepc_eprob`, etc as required.

###### Package links:
* [whoosh](http://pypi.python.org/pypi/Whoosh/)
* [django-haystack](http://haystacksearch.org/)
* [django_dajaxice](https://github.com/jorgebastida/django-dajaxice/tree/master/dajaxice)
* [django_dajax](https://github.com/jorgebastida/django-dajax/)
* [django_taggit on Pypi](https://pypi.python.org/pypi/django-taggit)
* [django_taggit docs](http://django-taggit.readthedocs.org/en/latest/)

##### 8. Running the Lighthouse server in typical usage 

_If you haven't started the djangostack services as described above, please do so before running Lighthouse (Cf. supra steps 2, 3)_.

To run, in lighthouse-taxonomy, do:
```
./scripts/run-lighthouse-new.sh
```

The script chooses a default port number (e.g., 8000 or 8080). You can use a different one, e.g., 8086, if you wish by supplying it:
```
./scripts/run-lighthouse-new.sh 8086
```

This HTTP server runs in the foreground of your shell and writes a log to the terminal.

Next, open a browser and type the URL below in the address bar of the browser 
(if you use a different port number other than 8000, specify the number in the URL)
```
http://127.0.0.1:8000/index/
```

If you see a generic Bitnami Djangostack webpage here (which usually serves on 8080), then there is a port number conflict. Restart lighthouse server with a different port number.

Have fun!

License
----
MIT License (MIT)
[OSI Approved License]
The MIT License (MIT)

Copyright (c) 2015 
University of Oregon and University of Colorado Boulder

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


