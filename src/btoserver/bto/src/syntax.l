%{
#include <math.h>
#include <string>
#include "syntax.hpp"
#include "syntax.tab.h"
%}

DIGIT     [0-9]
VAR       [a-zA-Z][a-zA-Z0-9]*

%option yylineno

%%

{DIGIT}+            { yylval.number = atoi(yytext); return (NUM); }
{DIGIT}+'.'{DIGIT}* { yylval.number = atof(yytext); return (NUM); }

"in" { return (IN); }
"inout" { return (INOUT); }
"out" { return (OUT); }
"row" { return (ROW); }
"column" { return (COLUMN); }
"vector" { return (VECTOR); }
"matrix" { return (MATRIX); }
"scalar" { return (SCALAR); }
"compressed" { return (COMPRESSED); }
"coordinate" { return (COORDINATE); }
"general" { return (GENERAL); }

{VAR}               { yylval.variable = new std::string(yytext); return(VAR); }

[':=+\-(){}*,]             { return *yytext; }


"{"[^}\n]*"}"       /* eat up one-line comments */

[ \t\n]+            /* eat up whitespace */

.                    printf( "Unrecognized character: %s\n", yytext );

%%
int yywrap() { return 1; }
