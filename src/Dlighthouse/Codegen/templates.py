import os, glob
from MatrixParser.matrixparser import MParser
from BTOClient.BTO import BTO_Client
from subprocess import call
from utils import remove_workdir

class BTOGenerator(object):
  ''' 
  BTO Code generator
  '''
  def __init__(self):
    pass

  def generateCode(self, userInput=''):
      defaultDir = os.getcwd()
      try:
          os.chdir('/tmp')
          if 'lighthouse_temp' in glob.glob('*'):
              os.system('rm -r lighthouse_temp')
          os.mkdir('lighthouse_temp')
          os.chdir('lighthouse_temp')
      except:
          os.chdir(defaultDir)
          return 'An error has occurred creating the temporary directory.'
          
      try:    
          filename = userInput.partition('\n')[0]
          filename = filename[:-1]+'.m'
          f = open(filename, 'w')
          f.write(userInput)
          f.close()
      except:
          remove_workdir('lighthouse_temp')
          os.chdir(defaultDir)
          return 'An error has occurred creating the BTO input file.'
          
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
      
      client = BTO_Client()
      host = 'localhost'
      port = 9999
      user = 'salin'
      options = '-e'
      try:
          client.submit_request(host, port, user, options, filename)
      except:
          remove_workdir('lighthouse_temp')
          os.chdir(defaultDir)
          return 'An error has occurred receiving the output file.'
          
      if 'errors.x' in glob.glob('*'):
          outputFile = 'errors.x'
      elif filename[:-2]+'.c' in glob.glob('*'):
          outputFile = filename[:-2]+'.c'
      else:
          remove_workdir('lighthouse_temp')
          os.chdir(defaultDir)
          return 'Unable to locate correct output file'
          
      Output = ''
      with open(outputFile, 'r') as f:
          for line in f:
              Output = Output + line

      remove_workdir('lighthouse_temp')
      os.chdir(defaultDir)
      return Output
