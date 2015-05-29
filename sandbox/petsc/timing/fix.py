#!/usr/bin/env python

import glob,os
for f in glob.glob("*.log"):
   stat = os.stat(f)
   fd = open(f,'r+')
   newlines = []
   lines = fd.readlines()
   #if not lines or lines[0].find('Hash: nohash') < 0: 
   for l in lines:
     if not l.startswith('2015-05'): newlines.append(l)
   hashnum = os.path.basename(f).split('.')[-2]
   #print f, hashnum
   
   if not lines or not newlines:
     fd.close()
     continue
   #if newlines[0].find('iccHash: 49598909') < 0: 
   if newlines[0].find('jacobiHash: nohash') < 0:
     fd.close()
     continue
   newlines.insert(0,'Hash: %s\n' % hashnum)
   newlines[1] = newlines[1].strip().replace('jacobiHash: nohash','jacobi') + newlines[2]
   del newlines[2]

   fd.seek(0)
   fd.write(''.join(newlines))
   fd.truncate()
   fd.close()
   os.utime(f, (stat.st_atime, stat.st_mtime))

