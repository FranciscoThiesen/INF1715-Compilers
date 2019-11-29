/* Victor Nogueira - 1511043 & Francisco Thiesen - 1611854 */

%token TK_INT TK_CHAR TK_FLOAT TK_BOOL
%token TK_IF TK_ELSE TK_WHILE TK_RET TK_NEW TK_AS
%token TK_AND TK_OR TK_PRINT TK_GEQUALS TK_LEQUALS TK_EQUALS TK_NEQUALS
%token <i> TK_RAWINT
%token <f> TK_RAWFLOAT
%token <str> TK_STRING TK_ID
%token <b> TK_TRUE TK_FALSE

%define parse.error verbose

%code requires {
#include "ast.h"

}

%{
#include <stdio.h>
#include "lex.yy.h"
#include "monga.tab.h"
#include "aux.h"

typedef struct func Func;
void yyerror(const char *);
%}

%union {
    Type *type;
    Exp *exp;
    Exp_list *elist;
    Stat *stat;
    Cmd *cmd;
    RefVar *ref;
    int i;
    float f;
    char *str;
    bool b;
    Def *def;
    int line;
}
%type <cmd> cmds cmd
%type <type> tipo_nativo tipo tipo_opt
%type <stat> stat
%type <exp> exp exp_and exp_comp exp_somasub exp_divmul exp_unary
            exp_atomic exp_base primitiva chamada_func exp_var
%type <ref> var
%type <elist> exps exp_tail
%type <def> def_var def_vars def_func params param param_tail defs def programa
%type <line> '(' ')' '+' '-' '*' '/' ':' ';' '=' '[' ']' '<' '>' '!' TK_AS TK_NEW TK_GEQUALS TK_EQUALS TK_LEQUALS TK_NEQUALS TK_OR TK_AND
%%

programa : defs                               { GLOBAL_TREE = $1; }
         | %empty                             { $$ = NULL; }

defs : def                                   { $$ = defseq($1, NULL); }
     | def defs                              { $$ = defseq($1, $2); }

def : def_var                                { $$ = $1; }
    | def_func                               { $$ = $1; }

def_var : TK_ID ':' tipo ';'                { $$ = vardef($1, $3, $4); }

def_vars : %empty                           { $$ = NULL; }
         | def_vars def_var                 { $$ = varseqdef($1, $2); };

def_func : TK_ID '(' params ')'
         tipo_opt stat                      { $$ = funcdef($1, $3, $5, $6, $2); }

tipo_nativo : TK_INT                        { $$ = newtype(INT); }
            | TK_CHAR                       { $$ = newtype(CHAR); }
            | TK_FLOAT                      { $$ = newtype(FLOAT); }
            | TK_BOOL                       { $$ = newtype(BOOL); }

tipo : tipo_nativo                          { $$ = $1; }
     | '[' tipo ']'                         { $$ = newseqtype($2); }

tipo_opt : %empty                           { $$ = NULL; }
         | ':' tipo                         { $$ = $2; }

params : %empty                             { $$ = NULL; }
       | param param_tail                   { $$ = varseqdef($1, $2); }

param : TK_ID ':' tipo                      { $$ = vardef($1, $3, $2); }

param_tail : %empty                         { $$ = NULL; }
           | ',' param param_tail           { $$ = varseqdef($2, $3); }

stat : '{' def_vars cmds '}'                { $$ = newstat($2, $3); }

cmds : %empty                               { $$ = NULL; }
     | cmd cmds                             { $$ = newseqcmd($1, $2); }

cmd : TK_IF '(' exp ')' stat                { $$ = newcmd(IF, $2, $3, $5, NULL); }
    | TK_IF '(' exp ')' stat TK_ELSE stat   { $$ = newcmd(IFELSE, $2, $3, $5, $7); }
    | TK_RET ';'                            { $$ = newcmd(RET, $2, NULL, NULL, NULL); }
    | TK_RET exp ';'                        { $$ = newcmd(RETEXP, $3, $2, NULL, NULL); }
    | TK_WHILE '(' exp ')' stat             { $$ = newcmd(WHILE, $2, $3, $5, NULL); }
    | TK_PRINT exp ';'                      { $$ = newcmd(PRINT, $3, $2, NULL, NULL); }
    | stat                                  { $$ = statcmd($1); }
    | chamada_func ';'                      { $$ = callcmd($1, $2); }
    | exp_var ';'                           { $$ = attcmd($1, $2); }

chamada_func : TK_ID '(' exps ')'           { $$ = callexp($1, $2, $3); }

exp_var : var '=' exp                       { $$ = attexp($1, $2, $3); };

exp : exp TK_OR exp_and                     { $$ = binaryexp(OR, $2, $1, $3); }
    | exp_and                               { $$ = $1; }

exp_and : exp_and TK_AND exp_comp           { $$ = binaryexp(AND, $2, $1, $3); }
        | exp_comp                          { $$ = $1; }

exp_comp : exp_comp TK_EQUALS exp_somasub   { $$ = binaryexp(EQ, $2, $1, $3); }
         | exp_comp TK_NEQUALS exp_somasub  { $$ = binaryexp(NEQ, $2, $1, $3); }
         | exp_comp TK_GEQUALS exp_somasub  { $$ = binaryexp(GEQ, $2, $1, $3); }
         | exp_comp TK_LEQUALS exp_somasub  { $$ = binaryexp(LEQ, $2, $1, $3); }
         | exp_comp '<' exp_somasub         { $$ = binaryexp(L, $2, $1, $3); }
         | exp_comp '>' exp_somasub         { $$ = binaryexp(G, $2, $1, $3); }
         | exp_somasub                      { $$ = $1; }

exp_somasub : exp_somasub '+' exp_divmul    { $$ = binaryexp(SUM, $2, $1, $3); }
            | exp_somasub '-' exp_divmul    { $$ = binaryexp(SUB, $2, $1, $3); }
            | exp_divmul                    { $$ = $1; }

exp_divmul : exp_divmul '*' exp_unary       { $$ = binaryexp(MUL, $2, $1, $3); }
           | exp_divmul '/' exp_unary       { $$ = binaryexp(DIV, $2, $1, $3); }
           | exp_unary                      { $$ = $1; }

exp_unary : '-' exp_unary                   { $$ = unaryexp(MINUS, $1, $2); }
          | '!' exp_unary                   { $$ = unaryexp(NOT, $1, $2); }
          | exp_atomic                      { $$ = $1; }

exp_atomic : exp_base                       { $$ = $1; }
           | exp_base TK_AS tipo            { $$ = asexp($1, $2, $3); }
           | TK_NEW tipo '[' exp ']'        { $$ = newexp($2, $3, $4); }

exp_base : primitiva                        { $$ = $1; }
         | chamada_func                     { $$ = $1; }

primitiva : TK_RAWINT                       { $$ = newint($1); }
          | TK_RAWFLOAT                     { $$ = newfloat($1); }
          | TK_STRING                       { $$ = newstr($1); }
          | TK_TRUE                         { $$ = newbool($1); }
          | TK_FALSE                        { $$ = newbool($1); }
          | var                             { $$ = varexp($1); }
          | '(' exp ')'                     { $$ = $2; }

exps : %empty                               { $$ = NULL; }
     | exp exp_tail                         { $$ = listexp($1, $2); }

exp_tail : %empty                           { $$ = NULL; }
         | ',' exp exp_tail                 { $$ = listexp($2, $3); }

var : exp_base '[' exp ']'                  { $$ = newvararr($1, $3, $2); }//binaryexp(VAR, $2, $1, $3); }
    | TK_ID                                 { $$ = newvarid($1); }

%%

void yyerror (char const *s) {
    fprintf (stderr, "%s in line %d\n", s, line);
}
