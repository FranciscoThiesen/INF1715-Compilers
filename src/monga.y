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
programa: defvariavel   {}
        | deffuncao     {}
defvariavel: TK_ID : tipo ';' {printf("def variavel\n");}
tipo: TK_INT {}
    | TK_CHAR {}
    | TK_BOOL {}
    | TK_FLOAT {}
    | '[' tipo  ']' {}
deffuncao: TK_ID '(' params ')' ':' tipo bloco {}
params: params params2 {}
params2: TK_ID ':' tipo ',' params2
       | TK_ID ':' tipo {}
bloco: '{' defvariaveis /*commandos*/ '}' {}
/*comandos: */
defvariaveis: defvariavel defvariaveis | defvariavel {}
%%
