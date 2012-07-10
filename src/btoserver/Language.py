import re,string,types

########################################################################
class C:
    """functions for parsing and manipulating C-code"""

    def __init__(self,text):
        self.text = text
        self.pos = 0

    def delete(self,x):
        # argument can be string or int
        start = self.pos 
        if type(x) == types.IntType:
            end = start+x    
        elif type(x) == types.StringType:
            end = start+len(x)
            if self.text[start:end]!=x:
                print "internal error, text pointer at",start,end,self.text[start:end]
                raise
        else:
            raise TypeError, "argument must be Int or String"
        self.text=self.text[:start]+self.text[end:]
        
    def insert(self,newstring):
        self.text=self.text[:self.pos]+newstring+self.text[self.pos:]
        
    def replace(self,oldstring,newstring):
        self.delete(oldstring)
        self.insert(newstring)

    def search_include(self):
        # returns startposition and endposition of the next include path
        # in the text. The position pointer (self.pos) is set to the
        # begin of the included path.
        # Include directives inside comments (/* ... */) are ignored.
        # Only include directives at the beginning of a line are
        # recognized (whitespaces allowed).
        comment = re.compile("/\*.*?\*/",re.DOTALL)
        include = re.compile(r"""        
        ^\s*                            # skip leading whitespaces
        \#include                       # keyword: include
        \s*                             # more whitespaces
        (
        (<(?P<path1>.*?)>)              # <path> 
        |                               # or
        (\"(?P<path2>.*?)\")            # "path"
        )""",re.MULTILINE|re.VERBOSE)
        result1 = include.search(self.text,self.pos)
        while result1:
            result2 = comment.search(self.text,self.pos)
            if result2 and result2.start()<result1.start():
                # result discarded because possibly inside comment
                # -> search for next include after the comment
                self.pos = result2.end()
                result1 = include.search(self.text,self.pos)
            else:
                if  result1.group('path1'):
                    (startpos,endpos) = result1.span('path1')
                if  result1.group('path2'):
                    (startpos,endpos) = result1.span('path2')
                self.pos = startpos
                return (startpos,endpos)
        return (-1,-1)

    def all_includefiles(self):
        # returns a list of all included files
        self.pos = 0
        l = []
        while 1:
            (start,end) = self.search_include()
            if start==-1: break
            l.append(self.text[start:end])
        return l
    
########################################################################
