%token TK_INT TK_CHAR TK_FLOAT TK_BOOL
%token TK_IF TK_ELSE TK_WHILE TK_RETURN TK_NEW TK_AS
%token TK_AND TK_OR TK_GEQUALS TK_LEQUALS TK_EQUALS TK_NEQUALS
%token TK_STRING
%token <wrap> TK_ID

%{
#include <stdio.h>
#include <stdlib.h>
%}
 
%%
 
programa : def_vars def_funcs
 
def_var : TK_ID ';' tipo ';'
 
def_vars : %empty
         | def_vars def_var
 
def_func : TK_ID '(' params ')' ':' tipo stat
 
def_funcs : %empty
          | def_func def_funcs
 
 
tipo_nativo : TK_INT
            | TK_CHAR
            | TK_FLOAT
            | TK_BOOL
 
tipo : tipo_nativo
     | tipo '[' ']'
 
params : %empty
       | param param_tail
 
param : TK_ID ':' tipo
 
param_tail : %empty
           | ',' param param_tail
 
stat : '{' def_vars cmds '}'
 
cmds : %empty
     | cmd cmds
 
cmd : TK_IF '(' exp ')' stat
    | TK_IF '(' exp ')' stat TK_ELSE stat
    | TK_RETURN ';'
    | TK_RETURN exp_or ';'
    | TK_WHILE '(' exp ')' stat
    | '@' exp ';'
    | stat
    | var '=' exp ';'
    | chamada_func ';'
 
chamada_func : TK_ID '(' exps ')'
 
exp : exp_or
 
exp_or : exp_or TK_OR exp_and
       | exp_and
 
exp_and : exp_and TK_AND exp_comp
        | exp_comp
 
exp_comp : exp_comp TK_EQUALS exp_somasub
         | exp_comp TK_NEQUALS exp_somasub
         | exp_comp TK_GEQUALS exp_somasub
         | exp_comp TK_LEQUALS exp_somasub
         | exp_comp '<' exp_somasub
         | exp_comp '>' exp_somasub
         | exp_somasub
 
exp_somasub : exp_somasub '+' exp_divmul
            | exp_somasub '-' exp_divmul
            | exp_divmul
 
exp_divmul : exp_divmul '*' exp_unary
           | exp_divmul '/' exp_unary
           | exp_unary
 
exp_unary : '-' exp_unary
          | '!' exp_unary
          | exp_atomic

exp_atomic : exp_base
           | exp_base TK_AS tipo
           | TK_NEW tipo '[' exp_somasub ']'

exp_base : primitiva
         | chamada_func

primitiva : TK_INT
          | TK_FLOAT
          | TK_STRING
          | TK_BOOL
          | var
          | '(' exp ')'

exps : %empty
     | exp_or exp_tail
 
exp_tail : %empty
         | ',' exp_or exp_tail
 
var : exp_base '[' exp_somasub ']'
    | TK_ID 
 
%%
