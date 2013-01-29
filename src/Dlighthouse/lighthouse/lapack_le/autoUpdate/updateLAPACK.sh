#!/bin/bash

PATH=/Applications/djangostack-1.4.1-0/python/bin: \
     /Applications/djangostack-1.4.1-0/mysql/bin: \
     /Library/Frameworks/Python.framework/Versions/2.7/bin: \
     /usr/bin: \
     /bin: \
     /usr/sbin: \
     /sbin: \
     /usr/local/bin: \
     /usr/local/git/bin: \
     /usr/texbin: \
     /usr/X11/bin

export PATH

###1. generate file names with current date and yesterday
date_yesterday=$(TZ=GMT+24 date +%m%d%Y)
date_today=$(date +%m%d%Y)
file_lapack_today=lapack_${date_today}.html
file_lapack_yesterday=lapack_${date_yesterday}.html


###2. wget download from http://www.netlib.org/lapack/lapack_routine/
wget -O $file_lapack_today http://www.netlib.org/lapack/lapack_routine/


###3. compare to the most curret files
diff $file_lapack_today lapack_current.html > diff.html

 
###4. if there is no change, rm today's file; otherwise, update 
if [ ! -s "diff.html" ]; then
	echo "empty"
        #rm $file_lapack_today
else
    # find the new routine's info, cut the paragraphs, and save to routineName.f 
    python urlFind.py
fi
