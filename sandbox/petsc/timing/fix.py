#!/usr/bin/env python

import glob,os

def writefile(fd, newlines, stat):
   fd.seek(0)
   fd.write(''.join(newlines))
   fd.truncate()
   fd.close()
   os.utime(f, (stat.st_atime, stat.st_mtime))

for f in glob.glob("*.log"):
   stat = os.stat(f)
   fd = open(f,'r+')
   newlines = []
   lines = fd.readlines()
   #if not lines or lines[0].find('Hash: nohash') < 0: 
   for l in lines:
     if not l.startswith('2015-05'): newlines.append(l)
     if l.startswith('-f /gpfs/mira-fs0/projects/PEACEndStation/norris/UFloridaSparseMat/petsc/'):
        matrixname = l.strip().split('/')[-1].replace('.petsc','')
   hashnum = os.path.basename(f).split('.')[-2]
   #print f, hashnum
   
   basename = os.path.basename(f)
   matname, solver, suffix = basename.split('.')
   if matname != matrixname:
     print "Renaming", f, "to", os.path.join(os.path.dirname(f), '.'.join([matrixname, solver, suffix]))
     fd.close()
     fd2 = open(os.path.join(os.path.dirname(f), '.'.join([matrixname, solver, suffix])),'w')
     writefile(fd2, newlines, stat)
     fd2.close()
     #os.remove(f)
     os.system('rm %s' % f)
      
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

   writefile(fd, newlines, stat)
