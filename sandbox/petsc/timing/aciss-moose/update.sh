#!/bin/bash
while true; do
  for f in `git ls-files -o` ; do 
     echo "Adding $f"
     git add $f 
  done
  git commit -m "more moose data" . && git pushh  
  sleep 60
done
