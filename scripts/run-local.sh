#!/bin/bash 

if [ "x$1" != "x" ]; then port=$1; else port=8080; fi

checkCmd() {
    # The first (required) argument is the type of failure, e.g., "FAIL", "XFAIL", "BROKEN"
    # The second (required) argument is the command
    # The third (optional) argument is the error message in case of failure
    # The fourth (optional) argument is a list of files which should exist upon successful completion of command.
    fail=$1
    cmd=$2
    if [ "x$3" != "x" ]; then
        msg=": $3"
    else
        msg=""
    fi
    files=""
    if [ "x$4" != "x" ]; then 
        files=$4
    fi
    echo "$cmd"
    msg=`$cmd`
    errcode=$?
    if [ "$errcode" != "0" ]; then
        echo "$fail($errcode)$msg."
  if [ "$fail" = "XFAIL" ]; then return 0; fi
        exit 1
    else
        for f in $files; do
          # When a file path begins with "!", check for non-existence, otherwise check for existence
          if test "${f:0:1}" = "!" ; then 
            if test -e "${f:1}" ; then 
              echo "$fail($errcode): file ${f:1} not renamed/removed."
              if test "$fail" = "XFAIL" ; then return 0; fi
                  exit 1
            fi
            elif ! test -e "$f" ; then 
                echo "$fail($errcode): file $f not created."
            if test "$fail" = "XFAIL" ; then return 0; fi
                exit 1
            fi
        done
        #echo "successful."
        return 0
    fi
}

pythonexec=python

dsversion=`ls /Applications | grep djangostack | sed -e 's|djangostack-\(.*\)/|\1|'`

if [ -e /Applications/$dsversion/python/bin/python ]; then 
  # Using djangostack
  pythonexec=/Applications/$dsversion/python/bin/python
fi
pwd=`pwd`
haystack_whoosh_path="HAYSTACK_WHOOSH_PATH = '""$pwd""/src/Dlighthouse/index.whoosh'"

checkCmd "FAIL" "cd src/Dlighthouse" "Could not find the Lighthouse directiony (src/Dlighthouse)" "src/Dlighthouse"

cd src/Dlighthouse #&& sed -i1 -e "s|django\.db\.backends\.mysql|django\.db\.backends\.sqlite3|" -e "s|HAYSTACK_WHOOSH_PATH = .*$|$haystack_whoosh_path|"  settings.py || echo "FAIL: Could not set the HAYSTACK_WHOOSH_PATH in src/Dlighthouse/settings.py"

$pythonexec manage.py runserver 127.0.0.1:$port || echo  "FAIL: Could not start the Lighthouse server"

cd $pwd

