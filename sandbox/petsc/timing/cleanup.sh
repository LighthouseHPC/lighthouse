#!/bin/bash
me \*.log -mmin +120 -size -1500c
find . -tnype f -name .\* -mmin +120 | xargs /bin/rm
find . -type f -name \*.log -mm +120 -size -1500c | xargs /bin/rm
