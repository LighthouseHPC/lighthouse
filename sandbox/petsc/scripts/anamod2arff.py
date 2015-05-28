#!/usr/bin/env python
'''
Created on Feb 17, 2015

@author: norris
'''
import re, sys, os, argparse, glob

perf = dict()

def readFeatures(dirname='anamod_features'):
    files=glob.glob(dirname + '/*.anamod')
    featuresre= re.compile(r'========([^=]+)========')
    features = dict()
    featurenames = None
    for filename in files:
        basename = os.path.basename(filename)
        matrixname = basename.split('.')[0]
        f = open(filename,'r')
        contents = f.read()
        f.close()
        features[matrixname] = dict()
        m = featuresre.search(contents)
        if not m: 
            print "Could not extract features for " + str(filename)
            continue
        
        print "Extracting features for " + str(filename)
        flines = m.group(1).split('\n')
        tmpdictstr = '{'
        for fl in flines:
            if fl.startswith(' '):
                nameval = fl.split(':')
                nameval = [n.strip().replace('>','').replace('<','') for n in nameval]
                tmpdictstr += " '" + nameval[0] + "' : " + nameval[1] + ','
        tmpdictstr.rstrip(',')
        tmpdictstr += '}'
        features[matrixname] = eval(tmpdictstr.replace('****',"'?'").replace('-nan',"'?'"))
       
    #print features 
    return features

def readPerfData(features,dirname):
    '''Log format excerpt (solver, preconditioner, convergence reason, time, tolerance)
    Beginning of each matrixname.solverhash.log file
    Hash: 43373443
    tcqmr | icc | -3 | 1.926556e+02 | 84.0693 | 10000
    ...
    '''
    global perf
    files = glob.glob(dirname+'/*.log')
    mintime = sys.float_info.max
    maxtime = 0
    perf = dict()
    for logfile in files:
        fname=os.path.basename(logfile)
        matrixname,hashid,_ = fname.split('.')
        if not matrixname in features.keys(): continue
        if not features[matrixname]: continue

        #print matrixname
        if matrixname not in perf.keys():
            perf[matrixname] = dict()

        solverID = hashid
        fd = open(fname,'r')
        lines = fd.readlines()
        if len(lines) < 2: continue
        data = [d.strip() for d in lines[1].split('|')]
            #solvername = data[0]+'_' + data[1]
            perf[matrixname][solverID] = data
            timestr =  perf[matrixname][solverID][3].strip()
            if not timestr or perf[matrixname][solverID][2] == 'Failed' \
                or timestr.startswith('Errorcode'):
                timestr = sys.float_info.max
            dtime = float(timestr)
            perf[matrixname][solverID][3] =str(dtime)
            if dtime < mintime: 
                print matrixname, solverID, features[matrixname]
                mintime = dtime
            if dtime > maxtime: maxtime = dtime
            solverID += 1

    perf['mintime'] = mintime
    perf['maxtime'] = maxtime
    
    return perf

'''
    @param lines list of lines (strings)
'''
def convertToARFF(features,perfdata,minbest,besttol,fairtol,includetimes=False):
    if not features: return ''
    buf = '@RELATION petsc_data\n'
    nbest = 0
    # Header info
    featurenames = None
    for k in features.keys():
        if features[k]: 
            featurenames = features[k].keys()
            break
    for feature in featurenames:
        buf += '@ATTRIBUTE %s NUMERIC\n' % feature
    if includetimes:
        buf += '@ATTRIBUTE TIME NUMERIC\n'
    #buf += '@ATTRIBUTE class {best,fair,worst}'
    buf += '@ATTRIBUTE class {good,bad}'
    buf += '\n@DATA\n\n'
    #print featureslist
    
    #buf+=','.join(featurenames)+'\n'
 
    for matrixname in features.keys():
        #print matrixname, features[matrixname]
        if not perfdata.get(matrixname) or not features[matrixname]: continue
        params = features[matrixname]

        for solverID in perfdata[matrixname].keys():
            # solver, preconditioner, convergence reason, time, tolerance
            dtime = float(perfdata[matrixname][solverID][3])
            if dtime != float("inf") and dtime <= (1.0+besttol) * perfdata['mintime']: 
                label = 'good'
                nbest += 1
            #elif dtime != float("inf") and dtime >= (1.0+fairtol) * perfdata['mintime']: label = 'fair'
            else: label = 'bad'
            for f in featurenames:
                v = params.get(f)
                if not v: v = '?'
                buf += str(v) + ','
            if includetimes:
                buf += str(dtime) + ','
            buf += label + '\n'
        
        buf = buf.replace('inf',str(sys.float_info.max))
        
    return buf

def writeToFile(buf, fname):
    with open(os.path.basename(fname) + '.arff', "w") as arff_file:
        arff_file.write(buf)
        arff_file.close()
    return 
        

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-f','--fdir', default = 'anamod_features',
                        help="The directory name containing Anamod feature files", type=str)
    parser.add_argument('-p','--pdir', default = 'data/raw_data/sequential_data',
                        help="The directory name containing solver performance data", type=str)
    parser.add_argument('-b', '--besttol', default=250,    # Bayes: 250 or less  
                        help='The tolerance for assigning "best" to a time, e.g.,'+\
                              'if 20 is specified, then all times within 20%% of the minimum'+\
                              'will be assigned the "best" label',
                        type=int)
    parser.add_argument('-m', '--minbest', default=200,
                        help='The minimum number of instances labeled with "best"')
    parser.add_argument('-r', '--fairtol', default=650,
                        help='The tolerance for assigning "fair" to a time, e.g.,'+\
                              'if 40 is specified, then all times greater than the "best"'+\
                              'criterion but within 40%% of the minimum'+\
                              'will be assigned the "fair" label',
                        type=int)
    parser.add_argument('-t', '--times', default=False, 
                        help='If True, include the minimum, maximum, and average times in the'+\
                              'features.', action='store_true')

    args = parser.parse_args()

    fdirname = args.fdir
    pdirname = args.pdir
    minbest = args.minbest
    besttol = args.besttol / 100.0
    fairtol = args.fairtol / 100.0
    includetimes = args.times   

    if not fdirname or not os.path.exists(fdirname): 
        print "Error: Please specify a valid directory containing Anamod feature files."
        parser.print_usage(sys.stderr)
        sys.exit(1)
        
    if not pdirname or not os.path.exists(pdirname): 
        print "Error: Please specify a valid directory containing solver performance data."
        parser.print_usage(sys.stderr)
        sys.exit(1)

    
    features = readFeatures(fdirname)
    perfdata = readPerfData(features,pdirname)
    buf = convertToARFF(features,perfdata,minbest,besttol,fairtol,includetimes)
    writeToFile(buf,'petsc')
    pass