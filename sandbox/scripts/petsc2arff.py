#!/usr/bin/env python
'''
Created on Feb 17, 2015
@author: norris
Modified by KS on April 17, 2017 for petsc features for the arff.
To run: python ../scripts/petsc2arff.py -T  /home/users/kanikas/research/lighthouse/sandbox/matrix_properties/uflorida-features1.csv -p /home/users/norris/UFloridaSparseMat/timing-arya16 -t -b 30 -n petsc_petsc_arya_p16_30
'''
import re, sys, os, argparse, glob, time, datetime, socket, random
from solvers import *
solveropts = getsolvers()

def readFeatures(dirname='anamod_features'):
    files=glob.glob(dirname + '/*.anamod')
    featuresre= re.compile(r'========([^=]+)========')
    features = dict()
    feature_times = dict()
    
    # features dictionary is indexed by matrix name
    # each value is a dictionary with keys corresponding to feature name and
    # value corresponding to feature value
    featurenames = None
    for filename in files:
        basename = os.path.basename(filename)
        matrixname = basename.split('.')[0]
        print matrixname
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
                valtime = nameval[1].split(',')
                val = valtime[0]
                if len(valtime)>1:
                    if not matrixname in feature_times.keys():
                        feature_times[matrixname] = dict()
                    feature_times[matrixname][nameval[0]] = valtime[1]
                tmpdictstr += " '" + nameval[0] + "' : " + val + ','
        tmpdictstr.rstrip(',')
        tmpdictstr += '}'
        features[matrixname] = eval(tmpdictstr.replace('****',"'?'").replace('-nan',"'?'").replace('inf',"float('Inf')"))

    #print feature_times
    return features, feature_times


def readFeaturesPETSc(path='uflorida-features1.csv'):
    fd = open(path,'r')
    lines = fd.readlines()
    fd.close()
    features = dict()


    # features dictionary is indexed by matrix name
    # each value is a dictionary with keys corresponding to feature name and
    # value corresponding to feature value
    featurenames = []
    for s in lines[0].strip().strip(',').split(',')[1:]:
        featurenames.append('"%s"' % s)
    for line in lines[1:]:
        contents = line.strip().strip(',').split(',')
        matrixname = contents[0].replace('.petsc','')
        features[matrixname] = dict()
        i = 0
        for c in contents[1:]:
            features[matrixname][featurenames[i]] = c.replace('unconverged','?').replace('-nan','?')
            i += 1

    print featurenames
    return features


def readPerfDataBelos(features,filename):
    matrices = open(filename).readlines()
    perf = dict()
    solvers = dict()
    missing = []
    for matrixline in matrices:
        # Log format, example line:
        # 1138_bus.mtx, Block CG, RELAXATION, unconverged, 1000, 0.035481
        data = [x.strip() for x in matrixline.split(',')]
        solverID = data[1]+ '*' + data[2]  # KSP, PC
        convergence = data[3]
        matrixname = data[0].split('.')[0]
        
        if not matrixname in features.keys():
            if matrixname not in missing: missing.append(matrixname)
            continue
        if not features[matrixname]: continue

        if matrixname not in perf.keys():
            perf[matrixname] = {'mintime':sys.float_info.max}
        perf[matrixname][solverID] = [data[1],data[2],data[3],data[-1]]
        
        if solverID not in solvers.keys():
            solvers[solverID] = 0
        solvers[solverID] += 1

        time = data[-1]
        if convergence.strip() == 'unconverged':
            time = str(sys.float_info.max)
        perf[matrixname][solverID][3] = time


        dtime = float(time)
        if dtime < perf[matrixname]['mintime']:
            #print matrixname, solverID, features[matrixname]
            perf[matrixname]['mintime'] = dtime

    print "No features found for these matrices, their solver times have not been included: ", ','.join(missing)
    print "Number of solvers considered: ", len(solvers.keys())
    return perf, solvers



def readPerfData(features,dirname,threshold):
    '''Log format excerpt (solver, preconditioner, convergence reason, time, tolerance)
    Beginning of each matrixname.solverhash.log file (more recent tests only)
Hash: 74995666
preonly | lu | reason=4 | time=4.778624e-03 | norm=2.67043e-11 | its=1 | p=38
    ...
    On other platforms, we need to extract this data from the petsc command line options, which
    are also in the log
    '''
    #solvers = getsolvers()
    files = glob.glob(dirname+'/*.log')
    #files = open(".bgq",'r').readlines()
    print "Reading performance data from", len(files), "files"
    perf = dict()
    solversamples = dict()
    
    
    count = 0
    for logfile in files:
	timefound = False
        #logfile = dirname + '/' + logfile.strip()
        print count, ": Processing", logfile
        count += 1
        statinfo = os.stat(logfile)
        
        fname=os.path.basename(logfile)
        parts = fname.split('.')
        pval = 'p1'
        if len(parts) == 4: matrixname,hashid,pval = parts[:3]
        else: matrixname,hashid = parts[:2]
        nprocs = pval.strip('p')

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
        if contents.find('-hash ') < 0: continue
        lines = contents.split('\n')
        norm = 0.0
        timeout = False
        if lines[1].count('|') > 4:
            summary = lines[1].split('|')
            for item in summary:
                if item.startswith('norm='):
                    # This is the 2-norm of Au - b where u is the solution vector
                    # so a large nonzero value indicates the solution was not
                    # computed successfully
                    norm = float(item.split('=')[-1].strip())
        if len(lines) < 2:  #Check for timeout
            if lines[-1].strip().endswith('timeout'):
                timeout = True
        if abs(norm) > 0.1 or timeout:
            perf[matrixname] = {'mintime':sys.float_info.max}
            solverpc = solveropts[solverID].split()
            s = solverpc[1]
            p = ' '.join(solverpc[3:])
            perf[matrixname][solverID] = [s,p,'-100',str(sys.float_info.max),str(sys.float_info.max),nprocs]
            continue

        bgqdata = []
        for l in lines:
          if l.startswith('Machine characteristics: Linux-2.6.32-431.el6.ppc64-ppc64-with-redhat-6.5-Santiago'):
             # This is a BG/Q log, which had some custom output
             bgqdata = [d.strip() for d in lines[1].split('|')]
             break
        #else:
        options=False
        #data [solver, preconditioner, convergence reason, time, tolerance, numprocs]
        data = ['','','','','','']
        for l in lines:
          tmp=''
          if options:
            if l.startswith('-ksp_type'):
              data[0] = l.split()[-1]
            elif l.startswith('-pc_type'):
              if data[1]: tmp=data[1]
              data[1] = l.split()[-1] + tmp
            elif l.startswith('-pc_'):
              if data[1]: tmp=data[1]
              data[1] = tmp + l.split()[-1]

          # ------
          if l.startswith("#PETSc Option Table entries:"):
            options=True
          elif l.startswith("#End of PETSc Option Table entries"):
            break
          elif l.startswith("MatSolve"):
            data[3] = l.split()[3]     # time
          if l.startswith("KSPSolve") and not timefound:
            data[3] = l.split()[3]
          else: continue

        if bgqdata: data[3] = bgqdata[3]
        data[5] = str(nprocs)
        print data
        #solvername = solvers[hashid]
        if len(data)>3:
          perf[matrixname][solverID] = data
        else:
          continue

        timestr =  perf[matrixname][solverID][3].strip()
        # Petsc sometimes gloms numbers together, e.g., 1.4687e-0414.3,
        # so we read just 3 chars past the e
        if timestr.count('.')>1: timestr = timestr[:timestr.find('e')+4]
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

    avgsamplespersolver = 0
    maxsamples = 0
    minsamples=100000000
    for s,t in solversamples.items():
        avgsamplespersolver += t
        if t > maxsamples:
            maxsamples = t
        if t < minsamples:
            minsamples = t
    
    if (len(solversamples.keys()) > 0):
        avgsamplespersolver = avgsamplespersolver / len(solversamples.keys())

    print "Average, min, max samples per solver: ", avgsamplespersolver, minsamples, maxsamples

    return perf, solversamples

'''
    @param lines list of lines (strings)
'''
def convertToARFF(features,perfdata,besttol,fairtol=0,solvers={}, solversamples={},
                  includetimes=False,usesolvers=False,extrainfo=False):
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
    if usesolvers and not solvers:
        solvers = getsolvers()
    if usesolvers:
        buf += '@ATTRIBUTE solver {%s}\n' % (','.join(['"'+x+'"' for x in solvers.keys()]))
        csvbuf += ', solver'
    if extrainfo:
        csvbuf += ', solver_name, prec_name, matrix_name'
    if fairtol > 0:
        buf += '@ATTRIBUTE class {good,fair,bad}'
    else:
        buf += '@ATTRIBUTE class {good,bad}'
    buf += '\n@DATA\n\n'
    csvbuf += ', class\n'
    #print featureslist
    
    #buf+=','.join(featurenames)+'\n'

    if not solvers: solvers = getsolvers()
    for matrixname in features.keys():
        #print matrixname, features[matrixname]
        if not perfdata.get(matrixname) or not features[matrixname]: continue
        params = features[matrixname]

        for solverID in solvers.keys():
            # solver, preconditioner, convergence reason, time, tolerance
            if not solverID in perfdata[matrixname].keys(): continue
            
            # Remove solvers for which we have fewer than 10 samples
            if solverID in solversamples.keys() and solversamples[solverID] < 10: continue
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
                buf += '"' + str(solverID) + '"' + ','
                csvbuf += '"'+ str(solverID) + '"' + ','
            if extrainfo:
                csvbuf += str(perfdata[matrixname][solverID][0]) + ', '
                csvbuf += str(perfdata[matrixname][solverID][1]) + ', '
                csvbuf += matrixname + ', '
            buf += label + '\n'
            csvbuf += label + '\n'

        buf = buf.replace('inf',str(sys.float_info.max))
        csvbuf = csvbuf.replace('inf',str(sys.float_info.max)).replace('?','0').replace('good','1').replace('bad','-1')

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
                        help="The path to the PETSc feature CSV file", type=str)
    parser.add_argument('-p','--pdir',
                        help="The directory name containing solver performance data (collection of matrixname.hash.log files", type=str)
    parser.add_argument('-B','--belos',
                        help="The CSV file containing Belos sovler timing results", type=str)
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
    parser.add_argument('-m', '--minpoints', default=10,
                        help='Minimum number of points per matrix (the data with fewer is discarded).',
                        type=int)
    parser.add_argument('-s', '--solvers', default=True,
                        help='If True, include the solver ID in the '+\
                        'features.', action='store_true')
    parser.add_argument('--feature_times', default = False, action='store_true',
                        help='If specified, produce a CSV file anamod_feauture_times.csv with feature timings')
    parser.add_argument('-e','--extra_csv_info', default=False,
                        help='Adds solver name, preconditioner name, and matrix name to csv output',
                        action='store_true')

    args = parser.parse_args()
    
    

    fdirname = args.fdir
    pdirname = args.pdir
    petscfeaturepath = args.tpath
    print petscfeaturepath
    belosfile = args.belos
    #minbest = int(args.minbest)
    besttol = args.besttol / 100.0
    if args.fairtol > 0:
        fairtol = args.fairtol / 100.0
        nclasses = 3
    else: fairtol = 0
    includetimes = args.times
    usesolvers = args.solvers
    extrainfo = args.extra_csv_info
    outfile = args.name
    solvers = {}
    solversamples = {}

    if (not fdirname or not os.path.exists(fdirname)) and \
        (not petscfeaturepath or not os.path.exists(petscfeaturepath)):
        print "Error: Please specify a valid directory containing Anamod feature files or the CSV file containing PETSc features."
        parser.print_usage(sys.stderr)
        sys.exit(1)
    
    if not ((pdirname and os.path.exists(pdirname)) or (belosfile and os.path.exists(belosfile))):
        print "Error: Please specify a valid directory containing solver performance data."
        parser.print_usage(sys.stderr)
        sys.exit(1)

    features = None
    feature_times = None

    if fdirname:
        features, feature_times = readFeatures(fdirname)

    if petscfeaturepath:
        print "Reading PETSc features"
        features = readFeaturesPETSc(petscfeaturepath)

    if not features:
        "You must specify either the -f or -T command-line options."
        parser.help()

    if pdirname:
        perfdata, solversamples = readPerfData(features,pdirname,args.minpoints)
    elif belosfile:
        perfdata, solvers = readPerfDataBelos(features,belosfile)


    print len(perfdata.keys())
    if args.feature_times and feature_times:
        import csv
        writer = csv.writer(open('anamod_feature_times.csv', 'wb'))
        first = True
        for matrixname, value in feature_times.items():
            if matrixname not in perfdata.keys(): continue
            if first:
                first = False
                writer.writerow([''] + value.keys() + ['Best solver time'])
            times = value.values()
            writer.writerow([matrixname] + times + [perfdata[matrixname]['mintime']])


    buf = '%% Generated on %s, ' % datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S')
    buf += ' %s: %s, ' % (socket.gethostname(), os.path.realpath(__file__))
    buf += 'Command: "%s"\n' % ' '.join(sys.argv)
    csvbuf = buf
    arff, csv = convertToARFF(features,perfdata,besttol,fairtol,solvers,solversamples,includetimes,usesolvers,extrainfo)
    buf += arff
    csvbuf += csv
    basefilename = outfile+'_%d' % (args.besttol)
    if fairtol > 0: basefilename += '_%d' % args.fairtol
    writeToFile(buf, basefilename)
    writeToFile(csvbuf, basefilename, suffix='.csv')

    pass
