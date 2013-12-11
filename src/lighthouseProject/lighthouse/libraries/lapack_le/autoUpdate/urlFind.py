import os, urllib, shutil

f = open("diff.html")

### make a string out of diff.html
st='%s'% f.read()

### get the file name for the routine info page on http://www.netlib.org/ 
fileName = st.split("\"")[1]


### establish the full url of the routine by replacing filename with http://www.netlib.org
url = "http://www.netlib.org/lapack/lapack_routine/"+fileName


### cut the relavant paragraphs and save to a file
page = urllib.urlopen(url)
copy_page= open(fileName[:-1]+"txt", "w")
flag = 1
while True:
    line = page.readline()[3:]
    if "=======================================" in line:
        break
    if "Purpose:" in line:
        flag = 0
    if "ingroup" in line:
        flag = 1
    if not flag and not line.startswith(fileName[:-2]):
        copy_page.write(line)
    

### obtain routine proprty
thePrecision = fileName[0]

# determin storage type 
if fileName[2] in ('e', 'y', 'o', 'r'):
    storageType = 'full'
if fileName[2] == 'b':
    storageType = 'band'
if fileName[2] == 'p':
    storageType = 'packed'
if fileName[2] == 't':
    storageType = 'tridiagonal'
    
# determin matrix type
if fileName[1] == 'g':
    matrixType = 'general'
if fileName[1] == 's':
    matrixType = 'symmetric'
if fileName[1] == 'h':
    matrixType = 'Hermitian'
if fileName[1] == 't':
    matrixType = 'triangular'
if fileName[1] == 'p' and fileName[0] in ('s', 'd'):
    matrixType = 'SPD'
if fileName[1] == 'p' and fileName[0] in ('c', 'z'):
    matrixType = 'HPD'
    
# determin routine catagory


print thePrecision, storageType, matrixType

copy_page.close()
f.close()



