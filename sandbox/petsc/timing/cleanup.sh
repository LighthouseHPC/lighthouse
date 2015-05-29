#!/bin/bash

find . -type f -name .\* -mmin +120 | xargs /bin/rm
find . -name \*.log -mmin +120 -size -1500c | xargs /bin/rm
