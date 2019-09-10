%token TK_INT TK_CHAR TK_FLOAT TK_BOOL
%token TK_IF TK_ELSE TK_WHILE TK_RETURN TK_NEW TK_AS
%token TK_AND TK_OR TK_GEQUALS TK_LEQUALS TK_EQUALS TK_NEQUALS
%token <wrap> TK_ID

/*%error-verbose */

%{
#include <stdio.h>
#include <stdlib.h>
%}

%%
programa : defvariavel   {}
         | deffuncao     {}

defvariavel : TK_ID : tipo ';' {printf("def variavel\n");}

tipo : TK_INT {}
     | TK_CHAR {}
     | TK_BOOL {}
     | TK_FLOAT {}
     | '[' tipo  ']' {}

deffuncao : TK_ID '(' params ')' ':' tipo bloco {}

params : params params2 {}

params2 : TK_ID ':' tipo ',' params2
        | TK_ID ':' tipo {}

bloco : '{' {defvariaveis} { comando }'}' {}

var : TK_ID | exp '[' exp ']'

comando : TK_IF exp bloco [ ELSE bloco ] 
        | TK_WHILE exp bloco
        | var '=' exp ';'
        | TK_RETURN [ exp ] ';'
        | chamada ';'
        | '@' exp ';'
        | bloco


exp : NUMERAL | LITERAL | TRUE | FALSE
    | var
    | '(' exp ')'
    | chamada
    | exp TK_AS tipo
    | TK_NEW tipo '[' exp ']'
    | '-' exp
    | exp '+' exp
    | exp '-' exp
    | exp '*' exp
    | exp '/' exp
    | exp TK_EQUALS exp
    | exp TK_NEQUALS exp
    | exp TK_LEQUALS exp
    | exp TK_GEQUALS exp
    | exp '<' exp
    | exp '>'
    | '!' exp
    | exp TK_AND exp
    | exp TK_OR exp 

chamada : ID '(' lista-exp ')'

lista-exp : [ exp { ',' exp } ]
defvariaveis: defvariavel defvariaveis | defvariavel {}
%%
