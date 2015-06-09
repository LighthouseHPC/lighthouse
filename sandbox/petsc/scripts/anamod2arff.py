#!/usr/bin/env python
'''
Created on Feb 17, 2015

@author: norris
'''
import re, sys, os, argparse, glob, time, datetime, socket
from solvers import *

def readFeatures(dirname='anamod_features'):
    files=glob.glob(dirname + '/*.anamod')
    featuresre= re.compile(r'========([^=]+)========')
    features = dict()
    # features dictionary is indexed by matrix name
    # each value is a dictionary with keys corresponding to feature name and
    # value corresponding to feature value
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
        features[matrixname] = eval(tmpdictstr.replace('****',"'?'").replace('-nan',"'?'").replace('inf',"float('Inf')"))
       
    #print features 
    return features

def readFeaturesTrilinos(path='tpetra_properties/12procs_results.csv'):
    fd = open(path,'r')
    lines = fd.readlines()
    fd.close()
    features = dict()
    # features dictionary is indexed by matrix name
    # each value is a dictionary with keys corresponding to feature name and
    # value corresponding to feature value
    featurenames = []
    for s in lines[0].strip().strip(',').split(',')[1:]:
        if s.startswith("Eigen"):
            featurenames.append('"%s Re"' % s)
            featurenames.append('"%s Im"' % s)
        else:
            featurenames.append('"%s"' % s)
    
    for line in lines[1:]:
        contents = line.strip().strip(',').split(',')
        matrixname = contents[0].replace('.mtx','')
        features[matrixname] = dict()
        i = 1
        for c in contents[2:]:
            if featurenames[i].startswith('"Eigen'):
                if c.find(':') > 0:
                    real,imaginary=c.split(':')
                    features[matrixname][featurenames[i]] = real
                    features[matrixname][featurenames[i+1]] = imaginary
                else:
                    features[matrixname][featurenames[i]] = '?'
                    features[matrixname][featurenames[i+1]] = '?'
                i += 1
            else:
                features[matrixname][featurenames[i]] = c.replace('unconverged','?').replace('-nan','?')
            i += 1
    return features

def readPerfData(features,dirname):
    '''Log format excerpt (solver, preconditioner, convergence reason, time, tolerance)
    Beginning of each matrixname.solverhash.log file
    Hash: 43373443
    tcqmr | icc | -3 | 1.926556e+02 | 84.0693 | 10000
    ...
    '''
    #solvers = getsolvers()
    files = glob.glob(dirname+'/*.log')
    perf = dict()
    solversamples = dict()
    for logfile in files:
        print "Processing", logfile
        statinfo = os.stat(logfile)
        
        fname=os.path.basename(logfile)
        matrixname,hashid,_ = fname.split('.')
        if not matrixname in features.keys():
            print "No features found for this matrix"
            continue
        if not features[matrixname]: continue

        #print matrixname
        if matrixname not in perf.keys():
            perf[matrixname] = {'mintime':sys.float_info.max}

        solverID = hashid
        fd = open(logfile,'r')
        contents = fd.read()
        fd.close()
        if contents.find('Hash: ') < 0: continue
        lines = contents.split('\n')
        if len(lines) < 2: continue
        data = [d.strip() for d in lines[1].split('|')]
        #print data
        #solvername = solvers[hashid]
        perf[matrixname][solverID] = data
        timestr =  perf[matrixname][solverID][3].strip()
        if not timestr or perf[matrixname][solverID][2] == 'Failed' \
            or timestr.startswith('Errorcode'):
            timestr = sys.float_info.max
        dtime = float(timestr)
        perf[matrixname][solverID][3] =str(dtime)
        if not solverID in solversamples.keys(): solversamples[solverID] = 0
        solversamples[solverID] += 1
        if dtime < perf[matrixname]['mintime']:
            #print matrixname, solverID, features[matrixname]
            perf[matrixname]['mintime'] = dtime

    count1, count2 = 0, 0
    threshold = 100
    perf2 = dict()
    if True:
        for matrixname, solverdata in perf.items():
            perf2[matrixname] = dict()
            for solverID, data in solverdata.items():
                if solverID in solversamples.keys() and solversamples[solverID] > threshold:
                    perf2[matrixname][solverID] = data
                    count1 += 1
                elif solverID == 'mintime':
                    perf2[matrixname][solverID] = data
                else:
                    count2 += 1
    else:
        perf2 = perf
    print ">%d matrices, <=%d matrices: " % (threshold,threshold), count1, count2
    return perf2

'''
    @param lines list of lines (strings)
'''
def convertToARFF(features,perfdata,besttol,fairtol=0,includetimes=False,usesolvers=False):
    if not features: return ''
    buf = '@RELATION petsc_data\n'
    csvbuf = ''
    nbest = 0
    # Header info
    featurenames = None
    for k in features.keys():
        if features[k]: 
            featurenames = features[k].keys()
            break
    for feature in featurenames:
        buf += '@ATTRIBUTE %s NUMERIC\n' % feature

    csvbuf = ','.join(featurenames)
    if includetimes:
        buf += '@ATTRIBUTE TIME NUMERIC\n'
        csvbuf += ', time'
    if usesolvers:
        solvers = getsolvers()
        buf += '@ATTRIBUTE solver {%s}\n' % (','.join(solvers.keys()))
        csvbuf += ', solver'

    if fairtol > 0:
        buf += '@ATTRIBUTE class {good,fair,bad}'
    else:
        buf += '@ATTRIBUTE class {good,bad}'
    buf += '\n@DATA\n\n'
    csvbuf += ', class\n'
    #print featureslist
    
    #buf+=','.join(featurenames)+'\n'
    solvers = getsolvers()
    for matrixname in features.keys():
        #print matrixname, features[matrixname]
        if not perfdata.get(matrixname) or not features[matrixname]: continue
        params = features[matrixname]

        for solverID in solvers.keys():
            # solver, preconditioner, convergence reason, time, tolerance
            if not solverID in perfdata[matrixname].keys(): continue
            dtime = float(perfdata[matrixname][solverID][3])
            if dtime != float("inf") and dtime <= (1.0+besttol) * perfdata[matrixname]['mintime']:
                label = 'good'
                nbest += 1
            elif fairtol>0 and dtime != float("inf") and dtime >= (1.0+fairtol) * perfdata[matrixname]['mintime']:
                label = 'fair'
            else: label = 'bad'
            for f in featurenames:
                v = params.get(f)
                if not v: v = '?'
                buf += str(v) + ','
                csvbuf += str(v) + ','
            if includetimes:
                buf += str(dtime) + ','
                csvbuf += str(dtime) + ','
            if usesolvers:
                buf += str(solverID) + ','
                csvbuf += str(solverID) + ','
            buf += label + '\n'
            csvbuf += label + '\n'
        
        buf = buf.replace('inf',str(sys.float_info.max))
        csvbuf = csvbuf.replace('inf',str(sys.float_info.max))
        
    return buf, csvbuf

def writeToFile(buf, fname, suffix='.arff'):
    with open(os.path.basename(fname) + suffix, "w") as arff_file:
        arff_file.write(buf)
        arff_file.close()
    return 
        

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-f','--fdir',
                        help="The directory name containing Anamod feature files", type=str)
    parser.add_argument('-T','--tpath',
                        help="The path to the TPetra feature CSV file", type=str)
    parser.add_argument('-p','--pdir', default = 'timing',
                        help="The directory name containing solver performance data (collection of matrixname.hash.log files", type=str)
    parser.add_argument('-b', '--besttol', default=25,    # Bayes: 25 or less
                        help='The tolerance for assigning "best" to a time, e.g.,'+\
                              'if 20 is specified, then all times within 20%% of the minimum'+\
                              'will be assigned the "best" label',
                        type=int)
    #parser.add_argument('-m', '--minbest', default=20,
    #                    help='The minimum number of instances labeled with "best"')
    parser.add_argument('-n', '--name', default='solvers',
                        help='Prefix for output file names', type=str)
    parser.add_argument('-r', '--fairtol', default=0,
                        help='The tolerance for assigning "fair" to a time, e.g.,'+\
                              'if 40 is specified, then all times greater than the "best"'+\
                              'criterion but within 40%% of the minimum'+\
                              'will be assigned the "fair" label',
                        type=int)
    parser.add_argument('-t', '--times', default=False, 
                        help='If True, include the minimum, maximum, and average times in the'+\
                              'features.', action='store_true')

    parser.add_argument('-s', '--solvers', default=False,
                        help='If True, include the solver ID in the '+\
                        'features.', action='store_true')

    args = parser.parse_args()
    
    

    fdirname = args.fdir
    pdirname = args.pdir
    trilinosfeaturepath = args.tpath
    #minbest = int(args.minbest)
    besttol = args.besttol / 100.0
    if args.fairtol > 0:
        fairtol = args.fairtol / 100.0
        nclasses = 3
    else: fairtol = 0
    includetimes = args.times
    usesolvers = args.solvers
    outfile = args.name

    if (not fdirname or not os.path.exists(fdirname)) and \
        (not trilinosfeaturepath or not os.path.exists(trilinosfeaturepath)):
        print "Error: Please specify a valid directory containing Anamod feature files or the CSV file containing Trilinos (TPetra) features."
        parser.print_usage(sys.stderr)
        sys.exit(1)
        
    if not pdirname or not os.path.exists(pdirname): 
        print "Error: Please specify a valid directory containing solver performance data."
        parser.print_usage(sys.stderr)
        sys.exit(1)

    features = None
    
    if fdirname:
        features = readFeatures(fdirname)
    if trilinosfeaturepath:
        features = readFeaturesTrilinos(trilinosfeaturepath)
    if not features:
        "You must specify either the -f or -T command-line options."
        parser.help()

    perfdata = readPerfData(features,pdirname)
    buf = '%% Generated on %s, ' % datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S')
    buf += ' %s: %s, ' % (socket.gethostname(), os.path.realpath(__file__))
    buf += 'Command: "%s"\n' % ' '.join(sys.argv)
    csvbuf = buf
    arff, csv = convertToARFF(features,perfdata,besttol,fairtol,includetimes,usesolvers)
    buf += arff
    csvbuf += csv
    basefilename = outfile+'_%d' % (args.besttol)
    if fairtol > 0: basefilename += '_%d' % args.fairtol
    writeToFile(buf, basefilename)
    writeToFile(csvbuf, basefilename, suffix='.csv')
    pass
