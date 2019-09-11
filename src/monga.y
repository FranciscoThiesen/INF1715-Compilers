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

programa : def_vars lista_func  

tipo_nativo : TK_BOOL
              TK_INT
              TK_CHAR
              TK_FLOAT

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

chamada_func : TK_ID '(' lista_exp ')'

lista_func : %empty
           | def_func lista_func

param : TK_ID ':' tipo

params : %empty
       | param params1

params1 : %empty
        | ',' param params1 

cmd : TK_IF '(' exp ')' stat
    | TK_IF '(' exp ')' stat TK_ELSE stat
    | TK_WHILE '(' exp ')' stat
    | chamada_func ';'
    | TK_RETURN ';'
    | TK_RETURN exp_or ';'
    | '@' exp ';'
    | var '=' exp
    | stat

lista_cmd : %empty
          | cmd lista_cmd

stat : '{' def_vars lista_cmd '}'

var : TK_ID
    | exp_basica '[' exp_somasub ']'

exp: exp_or

exp_or : exp_or TK_OR exp_and
       | exp_and

exp_and : exp_and TK_AND exp_igualdade
        | exp_igualdade

exp_igualdade : exp_relacao_de_ordem TK_EQUALS exp_relacao_de_ordem
              | exp_relacao_de_ordem TK_NEQUALS exp_relacao_de_ordem
              | exp_relacao_de_ordem

exp_relacao_de_ordem : exp_somasub '<' exp_somasub
                     | exp_somasub '>' exp_somasub
                     | exp_somasub TK_LEQUALS exp_somasub
                     | exp_somasub TK_GEQUALS exp_somasub
                     | exp_somasub

exp_somasub : exp_somasub '+' exp_divmul
            | exp_somasub '-' exp_divmul
            | exp_divmul

exp_divmul : exp_divmul '*' exp_unary
           | exp_divmul '/' exp_unary
           | exp_unary

exp_unary : '-' exp_unary
          | '!' exp_unary
          | exp_inicial

exp_nova : TK_NEW tipo '[' exp_somasub ']'

exp_basica : primitiva
           | chamada_func

exp_inicial : exp_basica
            | exp_basica TK_AS tipo
            | exp_nova

lista_exp : %empty
          | exp_or lista_exp1

lista_exp1 : %empty
           | ',' exp_or lista_exp1


%%
