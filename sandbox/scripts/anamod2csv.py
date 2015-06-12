#!/usr/bin/env python

# Convert Anamod and petsc log output to CSV file

import sys, os, glob, re



def usage():
    print '''Usage:
    anamod2csv <path_to_anamod_files>'''
    return

def is_float_number(s):
    try:
        float(s)
        return True
    except ValueError:
        pass
    return False    

def is_int_number(s):
    # Only positive ints are of interest
    m = re.match(r'^\d+$',s.strip())
    if m: return True
    else: return False
 
def create_csv(matrixdict, featureNames, filename):
    # Column labels 
    #buf = 'matrix,' + ','.join(matrixdict.get(matrixdict.keys()[0]).keys()) + '\n'
    buf = 'matrix,' + ','.join(featureNames) + '\n'
    badlist = ''
    for matrix, features in matrixdict.items():
        if len(features.values()) < 10: 
            badlist += matrix + ','
            continue
        buf += matrix  
        for f in featureNames: 
            val = features.get(f)
            if val:
                buf += ', ' + str(val)
            else:
                buf += ', ' + 'nan'
        buf += '\n'
    
    fd = open(filename,'w')
    fd.write(buf)
    fd.close()
    if badlist:
        fd = open('bad_' + filename,'w')
        fd.write(badlist[:-1] + '\n')
        fd.close()
    pass

def main():
    if len(sys.argv) < 2:
        usage()
        sys.exit(1)

    metricre = re.compile(r'\s*<(.+)> : <(.+)>')
    dirname = sys.argv[1]
    filelist = glob.glob('%s/*.anamod' % dirname)
    matrix = {}
    featureNames=set()
    #print filelist
    for afile in filelist:
        if not afile.endswith('.anamod'): continue
        matrixname = os.path.basename(afile).replace('.anamod','')
        fd = open(afile,'r')
        lines = fd.readlines()
        fd.close()
        features = False
        badMatrix = True
        for line in lines:
            if not features and line.startswith('Category'): 
                matrix[matrixname] = {}
                features = True
            if line.startswith('Category'):
                category = line.split(':')[1].strip().strip('<').strip('>') 
            if line.strip() == "========":
                features = False
                badMatrix = False
            if features:
                m = metricre.match(line.strip())
                if m:
                    name = m.group(1) + ':' + category
                    featureNames.add(name)
                    valuestr = m.group(2)
                    if is_int_number(valuestr):
                        value = int(valuestr)
                    elif is_float_number(valuestr):
                        value = float(valuestr)
                    else:
                        value = float('nan')
                    matrix[matrixname][name] = value
                    #print matrixname, name, value
        if badMatrix: print "NODATA:",matrixname
        else: print matrixname

        #print matrix[matrixname]
    create_csv(matrix,featureNames,'all_features.csv')
    pass
    

if __name__ == "__main__":
    main()
