#!/bin/bash
INPUT=$1
OLDIFS=$IFS
IFS=,
[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }
while read matrix solved
do
    if [ $solved -eq 0 ]
    then
        echo solving $matrix
        sed -i "s/${matrix}, 0/${matrix}, 1/g" "$1"
    else
        echo skipping $matrix
    fi
done < $INPUT
IFS=$OLDIFS
