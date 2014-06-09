import os, glob
parentdir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
os.sys.path.insert(0,parentdir)
#print parentdir			#/homes/salin/Lighthouse
from BTOServer.MatrixParser.matrixparser import MParser
from BTOServer.BTOClient.BTO import BTO_Client
from subprocess import call
from BTOServer.BTOClient.utils import remove_workdir

class BTOGenerator(object):
  def __init__(self):
    pass

  def generateTmpDir(self):
    defaultDir = os.getcwd()
    #print defaultDir              #defaultDir: /Users/salin/Documents/Lighthouse/Dlighthouse
    try:
        os.chdir('/tmp')
        if 'lighthouse_temp' in glob.glob('*'):
            os.system('rm -r lighthouse_temp')
        os.mkdir('lighthouse_temp')
        os.chdir('lighthouse_temp')
        #print "current work dir:", os.getcwd()          #current work dir: /tmp/lighthouse_temp
    except:
        os.chdir(defaultDir)
        return 'An error has occurred creating the temporary directory.'

          
#      Parser code needs to be updated since BTO syntax has changed.
#      mparser = MParser(debug=0,printToStderr=False)
#      theresult = None
#      try:
#          theresult = mparser.processString(userInput)
#      except:
#          return 'An error has occurred parsing the input.'
#      
#      if theresult and len(mparser.lex.errors) == 0:
#          try:
#              os.chdir('/tmp')
#              os.mkdir('lighthouse_temp')
#              os.chdir('lighthouse_temp')
#              temp_dir = os.getcwd()
#          except:
#              os.chdir(defaultDir)
#              return 'An error has occurred creating the temporary directory.'
#              
#          try:    
#              filename = userInput.partition('\n')[0]
#              filename = filename[:-1]+'.m'
#              f = open(filename, 'w')
#              f.write(userInput)
#              f.close()
#          except:
#              utils.remove_workdir(temp_dir)
#              os.chdir(defaultDir)
#              return 'An error has occurred creating the BTO input file.'
#          
#      else:
#          return 'Syntax Errors\n', '\n'.join(mparser.lex.errors)

  def submitToBTO(self, filename):
      client = BTO_Client()
      host = 'localhost'
      port = 9999
      user = 'salin'
      #no need to split up options string
      options = '--level1 cache64:512:8'#thread2:12:2' #'-e'
      filename = str(filename)

      try:
          client.submit_request(host, port, user, options, filename)
      except:
          remove_workdir('lighthouse_temp')
          os.getenv("HOME")
          return 'An error has occurred receiving the output file.'
          
      if 'errors.x' in glob.glob('*'):
          outputFile = 'errors.x'
      elif filename[:-2]+'.c' in glob.glob('*'):
          outputFile = filename[:-2]+'.c'
      else:
          remove_workdir('lighthouse_temp')
          os.getenv("HOME")
          return 'Unable to locate correct output file'
          
      Output = ''
      with open(outputFile, 'r') as f:
          for line in f:
              Output = Output + line
     
      remove_workdir('lighthouse_temp')
      return Output
