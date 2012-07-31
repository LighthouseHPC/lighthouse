import os, shutil
from MatrixParser.matrixparser import MParser
from BTOClient.BTO import BTO_Client

class BTOGenerator(object):
  ''' 
  BTO Code generator
  '''
  def __init__(self):
    pass

  def generateCode(self, userInput=''):
      mparser = MParser(debug=0,printToStderr=False)
      theresult = None
      try:
          theresult = mparser.processString(userInput)
      except:
          pass
      
      if theresult and len(mparser.lex.errors) == 0:
          try:
              os.mkdir('temp')
              os.chdir('temp')
              cwd = os.getcwd()
              filename = userInput.partition('\n')[0]
              filename = filename+'.m'
              with open(filename, 'w') as f:
                  f.write(userInput)
          except:
              return 'System Error'
      else:
          return '*** Errors\n', '\n'.join(mparser.lex.errors)
      
      client = BTO_Client()
      host = 'localhost'
      port = 9999
      user = 'dljohnso'
      options = '-e'
      try:
          client.submit_request(host, port, user, options, filename)
      except:
          return 'System Error'
          
      outputFile = filename[:-2]+'.c'
      Output = ''
      with open(outputFile, 'r') as f:
          for line in f:
              Output = Output + line
      os.chdir('..')
      shutil.rmtree(cwd)
      return Output

