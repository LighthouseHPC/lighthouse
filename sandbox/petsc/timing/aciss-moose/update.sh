#!/bin/bash
while true; do
  for f in `git ls-files -o` ; do 
     echo "Adding $f"
     git add $f 
  done
  git commit -m "more moose data" . && git push  
  sleep 60
done
