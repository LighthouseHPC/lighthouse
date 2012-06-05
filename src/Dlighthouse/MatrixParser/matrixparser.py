#!/usr/bin/env python
'''
Created on Mar 8, 2012

@author: norris
'''

import os,sys
from MatrixParser.lexer import *
from MatrixParser.parser import *

class MatrixParser:
  ''' 
  BTO Parser
  '''
  
  def __init__(self, debug=0, outputdir='.', printToStderr=True):
    import MatrixParser.parser as matrixparser
    self.parser = matrixparser.setup(debug=debug, outputdir=outputdir)
    self.lex = MatrixLexer()
    self.lex.build(printToStderr=printToStderr, optimize=1, 
                   lextab=os.path.join("MatrixParser.lextab"))
    self.errorlog = []
    self.debug = debug
    
  def processString(self, input=''):
    if s == '' or s.isspace(): 
      return None
    else:
      return self.parser.parse(s, lexer=self.lex.lexer, debug=self.debug)

  def processFile(self, inputfile=''):
    if not os.path.exists(inputfile):
      self.error(0,"Input file not found: %s" % inputfile)
      return None
    else:
      f = open(sys.argv[i],"r")
      s = f.read()
      f.close()

      return self.parser.parse(s, lexer=self.lex.lexer, debug=self.debug)
  

  def error(self, msg):
    self.errorlog.append(msg)
    if printToStderr:
      print >>sys.stderr, msg


        
if __name__ == '__main__':
  
  global __matrix_language_vars
  
  mparser = MatrixParser(debug=0,printToStderr=True)
  
  for i in range(1, len(sys.argv)):
    print >>sys.stderr, "[parse] About to parse %s" % sys.argv[i]
    os.system('cat %s' % sys.argv[i])
    theresult = mparser.processFile(sys.argv[i])
    if theresult:
      print >>sys.stdout, '[parser] Successfully parsed %s' % sys.argv[i]
    
    print 'All variables and their types:'
    for key,val in getVars().items():
      print "%s : %s" % (key,val)


