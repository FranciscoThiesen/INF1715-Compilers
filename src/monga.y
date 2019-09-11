%token TK_INT TK_CHAR TK_FLOAT TK_BOOL
%token TK_IF TK_ELSE TK_WHILE TK_RETURN TK_NEW TK_AS
%token TK_AND TK_OR TK_GEQUALS TK_LEQUALS TK_EQUALS TK_NEQUALS
%token TK_STRING
%token <wrap> TK_ID

/*%error-verbose */

%{
#include <stdio.h>
#include <stdlib.h>
%}

%%

programa : def_vars 

tipo_nativo : TK_BOOL
              TK_INT
              TK_CHAR
              TK_FLOAT
              TK_VOID

tipo : tipo_nativo
     | tipo '[' ']'


primitiva : numeral 
          | palavra
          | var
          | '(' exp ')'

numeral : TK_INT
        | TK_FLOAT

palavra : TK_STRING

def_var : TK_ID ':' tipo ';'  

def_vars : %empty
         | def_vars def_var

def_func : TK_ID '(' params ')' ':' tipo stat

params : %empty
       | params params1

params1 : %empty
        | ',' params params1 


%%
