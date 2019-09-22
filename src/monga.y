/* Victor Nogueira - 1511043 & Francisco Thiesen - 1611854 */

%token TK_INT TK_CHAR TK_FLOAT TK_BOOL
%token TK_IF TK_ELSE TK_WHILE TK_RET TK_NEW TK_AS
%token TK_AND TK_OR TK_PRINT TK_GEQUALS TK_LEQUALS TK_EQUALS TK_NEQUALS
%token <i> TK_RAWINT
%token <d> TK_RAWFLOAT
%token <str> TK_STRING TK_ID
%token <c> TK_LITERAL TK_TRUE TK_FALSE

%define parse.error verbose

%{
#include <stdio.h>
#include "lex.yy.h"
#include "monga.tab.h"
#include "aux.h"
#include "ast.h"

void yyerror(const char *);
%}

%union {
    union type *type;
    union exps *exp;
    struct stat *stat;
    union cmd *cmd;
    struct param *param;
    struct func *func;
    struct var *var;
    int i;
    double d;
    char *str;
    char c;
    //int line;
}
%type <cmd> cmds cmd
%type <type> tipo_nativo tipo tipo_opt
%type <var> def_var def_vars
%type <func> def_func def_funcs
%type <param> params param param_tail
%type <stat> stat
%type <exp> exps exp_or exp_and exp_comp exp_somasub exp_divmul exp_unary
            exp_atomic exp_base primitiva exp_tail var exp chamada_func exp_var
             

%%
 
programa : def_vars def_funcs               { GLOBAL_TREE =  prognode($1, $2); }


def_var : TK_ID ':' tipo ';'                { $$ = vardef($1, $3); }

def_vars : %empty                           { $$ = NULL; }
         | def_vars def_var                 { $$ = varseqdef($1, $2); };

def_func : TK_ID '(' params ')'
         tipo_opt stat                      { $$ = func($1, $3, $5, $6) ;}

def_funcs : %empty                          { $$ = NULL; }
          | def_func def_funcs              { $$ = funcseq($1, $2) ;}

tipo_nativo : TK_INT                        { $$ = newtype(INT); }
            | TK_CHAR                       { $$ = newtype(CHAR); }
            | TK_FLOAT                      { $$ = newtype(FLOAT); }
            | TK_BOOL                       { $$ = newtype(BOOL); }

tipo : tipo_nativo                          { $$ = $1; }
     | '[' tipo ']'                         { $$ = newseqtype($2); }

tipo_opt : %empty                           { $$ = NULL; }
         | ':' tipo                         { $$ = $2; }
 
params : %empty                             { $$ = NULL; }
       | param param_tail                   { $$ = newparamseq($1, $2); }
 
param : TK_ID ':' tipo                      { $$ = newparam($1, $3); }
 
param_tail : %empty                         { $$ = NULL; }
           | ',' param param_tail           { $$ = newparamseq($2, $3); }
 
stat : '{' def_vars cmds '}'                { $$ = newstat($2, $3); }
 
cmds : %empty                               { $$ = NULL; }
     | cmd cmds                             { $$ = newseqcmd($1, $2); }
 
cmd : TK_IF '(' exp ')' stat                { $$ = newcmd(IF, $3, $5, NULL); }
    | TK_IF '(' exp ')' stat TK_ELSE stat   { $$ = newcmd(IFELSE, $3, $5, $7); }
    | TK_RET ';'                            { $$ = newcmd(RET, NULL, NULL, NULL); }
    | TK_RET exp_or ';'                     { $$ = newcmd(RETEXP, $2, NULL, NULL); }
    | TK_WHILE '(' exp ')' stat             { $$ = newcmd(WHILE, $3, $5, NULL); }
    | TK_PRINT exp ';'                      { $$ = newcmd(PRINT, $2, NULL, NULL); }
    | stat                                  { $$ = statcmd($1); }
    | chamada_func ';'                      { $$ = callcmd($1); }
    | exp_var ';'                           { $$ = attcmd($1); }
 
chamada_func : TK_ID '(' exps ')'           { $$ = callexp($1, $3); }
 
exp_var : var '=' exp                       { $$ = binaryexp(VAR, $1, $3); };

exp : exp_or                                { $$ = $1; }
 
exp_or : exp_or TK_OR exp_and               { $$ = binaryexp(OR, $1, $3); }
       | exp_and                            { $$ = $1; }
 
exp_and : exp_and TK_AND exp_comp           { $$ = binaryexp(AND, $1, $3); }
        | exp_comp                          { $$ = $1; }
 
exp_comp : exp_comp TK_EQUALS exp_somasub   { $$ = binaryexp(EQ, $1, $3); }
         | exp_comp TK_NEQUALS exp_somasub  { $$ = binaryexp(NEQ, $1, $3); }
         | exp_comp TK_GEQUALS exp_somasub  { $$ = binaryexp(GEQ, $1, $3); }
         | exp_comp TK_LEQUALS exp_somasub  { $$ = binaryexp(LEQ, $1, $3); }
         | exp_comp '<' exp_somasub         { $$ = binaryexp(L, $1, $3); }
         | exp_comp '>' exp_somasub         { $$ = binaryexp(G, $1, $3); }
         | exp_somasub                      { $$ = $1; }

exp_somasub : exp_somasub '+' exp_divmul    { $$ = binaryexp(SUM, $1, $3); }
            | exp_somasub '-' exp_divmul    { $$ = binaryexp(SUB, $1, $3); }
            | exp_divmul                    { $$ = $1; }
 
exp_divmul : exp_divmul '*' exp_unary       { $$ = binaryexp(MUL, $1, $3); }
           | exp_divmul '/' exp_unary       { $$ = binaryexp(DIV, $1, $3); }
           | exp_unary                      { $$ = $1; }
 
exp_unary : '-' exp_unary                   { $$ = unaryexp(MINUS, $2); }
          | '!' exp_unary                   { $$ = unaryexp(NOT, $2); }
          | exp_atomic                      { $$ = $1; }

exp_atomic : exp_base                       { $$ = $1; }
           | exp_base TK_AS tipo            { $$ = asexp($1, $3); }
           | TK_NEW tipo '[' exp_somasub ']'{ $$ = newexp($2, $4); }

exp_base : primitiva                        { $$ = $1; }
         | chamada_func                     { $$ = $1; }

primitiva : TK_RAWINT                       { $$ = newint($1); }
          | TK_RAWFLOAT                     { $$ = newfloat($1); }
          | TK_STRING                       { $$ = newstr($1); }
          | TK_TRUE                         { $$ = newbool($1); }
          | TK_FALSE                        { $$ = newbool($1); }
          | TK_LITERAL                      { $$ = newchar($1); }
          | var                             { $$ = $1; }
          | '(' exp ')'                     { $$ = $2; }

exps : %empty                               { $$ = NULL; }
     | exp_or exp_tail                      { $$ = listexp($1, $2); }
 
exp_tail : %empty                           { $$ = NULL; }
         | ',' exp_or exp_tail              { $$ = listexp($2, $3); }
 
var : exp_base '[' exp_somasub ']'          { $$ = binaryexp(VAR, $1, $3); }
    | TK_ID                                 { $$ = newvarid($1); }
 
%%

void yyerror (char const *s) {
    fprintf (stderr, "%s in line %d\n", s, line);
}
