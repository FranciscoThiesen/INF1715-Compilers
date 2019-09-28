#include "ast.h"
#include <stdio.h>
#define TABSTOP 4

static void print_stat( int n_spaces, struct stat *stat);
static void print_exp( int n_spaces, union exps *exp );

static void print_spaces( int n, int line_skip ) {
    if( line_skip ) printf("\n");
    for(int i = 0; i < n; ++i) {
        printf(" ");
    }
}

static void print_native_type( enum native_types type ) {
    switch( type ) {
        case INT:
            printf("INT");
            break;
        case CHAR:
            printf("CHAR");
            break;
        case FLOAT:
            printf("FLOAT");
            break;
        case BOOL:
            printf("BOOL");
            break;
        default:
            fprintf(stderr, "unknown type\n");
            exit(-1);
    }
}
 
static void print_type( int n_spaces, union type* type) {
    if( type != NULL ) {
        if( type->tag == SINGLE ) {
            print_native_type( type->single.type );
        }
        else {
            printf("[");
            print_type( 0, type->seq.next );
            printf("]");
        }
    }
}
 
static void print_var( int n_spaces, struct var *v ) {
    if( v != NULL ) {
        print_spaces( n_spaces, 0);
        print_type( 0, v->type );
        printf(" %s\n", v->name );
        print_var( n_spaces, v->next );
    }
}

static void print_params( struct param* param ) {
    if( param == NULL ) return;
    printf("%s ", param->name );
    print_type( 0, param->type );
    if( param->next != NULL ) {
        printf(", ");
        print_params( param->next );
    }
}

static void print_expvarid( int n_spaces, union exps *exp_var ) {
    printf("SIMPLEVAR = %s", exp_var->var.name );
}

static void print_call( int n_spaces, union exps *exp ) {

    printf("CALL %s", exp->call.name);
    print_spaces( n_spaces, 1);
    printf("ARGS {\n");
    print_exp( n_spaces + TABSTOP, exp->call.listexp );
    print_spaces( n_spaces, 1);
    printf("}\n");
}

static void print_new( int n_spaces, union type *type, union exps *exp) {
    printf("NEW {\n");
    print_spaces( n_spaces + TABSTOP, 0);
    printf("TYPE ");
    print_type( 0, type );
    printf(" [\n");
    print_exp( n_spaces + TABSTOP, exp );
    print_spaces( n_spaces + TABSTOP, 1);
    printf("]\n");
    print_spaces ( n_spaces, 1);
    printf("}");
}

static void print_as( int n_spaces, union exps *exp, union type *type) {
    printf("AS {");
    print_spaces( n_spaces + TABSTOP, 1);
    printf("TYPE ");
    print_type( 0, type );
    print_exp( n_spaces + TABSTOP, exp );
    print_spaces ( n_spaces, 1);
    printf("}");
}

static void print_expatt( int n_spaces, union exps *e1, union exps *e2) {
    printf("ASSIGN {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    printf("\n");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_or( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("OR {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_and( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("AND {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_not( int n_spaces, union exps *e1 ) {
    printf("NOT {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_eq( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("EQUALS {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_neq( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("NOT EQUAL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts(""); 
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_geq( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("GREATER OR EQUAL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}


static void print_leq( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("LESS OR EQUAL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts(""); 
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}


static void print_less( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("LESS {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_greater( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("GREATER {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_sum( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("SUM {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_sub( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("SUB {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_mul( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("MUL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_div( int n_spaces, union exps *e1, union exps *e2 ) {
    printf("DIV {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_expvar( int n_spaces, union exps *e1, union exps *e2) {
    printf("NOTSOSIMPLEVAR {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_minus( int n_spaces, union exps *e1 ) {
    printf("MINUS {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_exp( int n_spaces, union exps *exp ) {
    if( exp != NULL ) {
        print_spaces( n_spaces, 0);
        switch( exp->tag ) {
            case VAR:
                print_expvar( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next ) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case VARID:
                print_expvarid( n_spaces, exp );
                if( exp->var.next ) puts("");
                print_exp( n_spaces, exp->var.next );
                break;
            case CALLEXP:
                print_call( n_spaces, exp );
                if( exp->call.next ) puts("");
                print_exp( n_spaces, exp->call.next );
                break;
            case AS:
                print_as (n_spaces, exp->as.exp, exp->as.type);
                if( exp->as.next ) puts("");
                print_exp( n_spaces, exp->as.next );
                break;
            case NEW:
                print_new( n_spaces, exp->new.type, exp->new.exp );
                if( exp->new.next ) puts("");
                print_exp( n_spaces, exp->new.next );
                break;
            case SUM:
                print_sum( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case SUB:
                print_sub( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case MINUS:
                print_minus( n_spaces, exp->unary.exp );
                print_exp( n_spaces, exp->unary.next );
                break;
            case MUL:
                print_mul( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case DIV:
                print_div( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case EXPINT:
                printf("INT %d", exp->expint.i );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->expint.next );
                break;
            case EXPFLOAT:
                printf("FLOAT %lf", exp->expfloat.d );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->expfloat.next );
                break;
            case EXPCHAR:
                printf("CHAR %c", exp->expchar.c );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->expchar.next );
                break;
            case EXPSTR:
                printf("STRING %s", exp->expstr.str );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->expstr.next );
                break;
            case EXPBOOL:
                printf("BOOL ");
                printf( exp->expbool.b ? "TRUE" : "FALSE" );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->expbool.next );
                break;
            case EXPATT:
                print_expatt ( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case EQ:
                print_eq( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case NEQ:
                print_neq( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case LEQ:
                print_leq( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case GEQ:
                print_geq( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case L:
                print_less( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case G:
                print_greater( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case NOT:
                print_not( n_spaces, exp->unary.exp );
                if( exp->unary.next ) puts("");
                print_exp( n_spaces, exp->unary.next );
                break;
            case AND:
                print_and( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next ) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            case OR:
                print_or ( n_spaces, exp->binary.e1, exp->binary.e2 );
                if( exp->binary.next) puts("");
                print_exp( n_spaces, exp->binary.next );
                break;
            default:
                fprintf(stderr, "Unknown expression %d!\n", exp->tag);
                exit(-1);
        }
    }
}

static void print_if( int n_spaces, union exps *exp, struct stat *stat ) {
    print_spaces(n_spaces, 0);
    printf("IF (\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf(")");
    print_spaces( n_spaces, 1);
    print_stat( n_spaces, stat );
}

static void print_else( int n_spaces, struct stat *stat ) {
    print_spaces( n_spaces, 0 );
    printf("ELSE\n");
    print_stat( n_spaces, stat );
}


static void print_ifelse ( int n_spaces, union exps *exp, struct stat *stat, struct stat *stat2 ) {
    print_if( n_spaces, exp, stat );
    print_else( n_spaces, stat2 );
    print_spaces( n_spaces, 0);
}

static void print_ret (int n_spaces) {
    print_spaces( n_spaces, 0 );
    printf("RET\n");
}

static void print_retexp( int n_spaces, union exps *exp ) {
    print_spaces( n_spaces, 0 );
    printf("RETEXP {\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_while ( int n_spaces, union exps *exp, struct stat *stat ) {
    print_spaces( n_spaces, 0 );
    printf("WHILE (\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf(")\n");
    if(stat) print_spaces( n_spaces, 0);
    print_stat( n_spaces, stat );
}

static void print_print( int n_spaces, union exps *exp ) {
    print_spaces( n_spaces, 0 );
    printf("PRINT {\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_cmd( int n_spaces, union cmd *cmd ) {
    if (!cmd)
        return;
    //print_spaces( n_spaces, 0);
    switch ( cmd->tag ) {
        case IF:
            print_if( n_spaces, cmd->cmd_if.exp, cmd->cmd_if.stat);
            if( cmd->cmd_if.next ) printf("\n");
            print_cmd( n_spaces, cmd->cmd_if.next);
            break;
        case IFELSE:
            print_ifelse( n_spaces, cmd->cmd_ifelse.exp, cmd->cmd_ifelse.stat, cmd->cmd_ifelse.stat2 );
            if( cmd->cmd_ifelse.next) printf("\n");
            print_cmd( n_spaces, cmd->cmd_ifelse.next );
            break;
        case RET:
            print_ret( n_spaces );
            if( cmd->cmd_ret.next) printf("\n");
            print_cmd( n_spaces, cmd->cmd_ret.next );
            break;
        case RETEXP:
            print_retexp( n_spaces, cmd->cmd_ret_exp.exp);
            if( cmd->cmd_ret_exp.next) printf("\n"); 
            print_cmd( n_spaces, cmd->cmd_ret_exp.next );
            break;
        case WHILE:
            print_while( n_spaces, cmd->cmd_while.exp, cmd->cmd_while.stat);
            if( cmd->cmd_while.next) printf("\n");
            print_cmd( n_spaces, cmd->cmd_while.next );
            break;
        case PRINT:
            print_print( n_spaces, cmd->print.exp);
            if( cmd->print.next) printf("\n");
            print_cmd( n_spaces, cmd->print.next );
            break;
        case CALLCMD:
            print_exp( n_spaces, cmd->call.call );
            if( cmd->call.next ) printf("\n");
            print_cmd( n_spaces, cmd->call.next );
            break;
        case ATTCMD:
            print_exp( n_spaces, cmd->att.att );
            if( cmd->att.next ) printf("\n");
            print_cmd( n_spaces, cmd->att.next );
            break;
        case STAT:
            print_cmd( n_spaces, cmd->stat.next);
            break;
        default:
            fprintf(stderr, "Unknown command!\n");
            exit(-1);
    }
}

static void print_stat( int n_spaces, struct stat *stat ) {
    if (!stat)
        return;
    printf("{\n");
    print_var( n_spaces + TABSTOP, stat->vars);
    print_cmd( n_spaces + TABSTOP, stat->cmds);
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_func( int n_spaces, struct func* f ) {
    if( f != NULL ) {
        print_spaces( n_spaces, 1);
        printf("FUNC< ");
        print_type( 0, f->type );
        printf(" > ");
        printf(" %s, ", f->name );
        printf("PARAMS = {");
        print_params( f->param );
        printf("}\n");
        if( f->stat ) {
            print_spaces( n_spaces, 0);
            print_stat( n_spaces, f->stat );
        }
        if( f->next ) {
            print_spaces( n_spaces, 1);
            print_func( n_spaces, f->next );
        }
    }
}

void print_defs(union def *def) {
    if (!def)
        return;

    switch (def->tag) {
        case DEFVAR:
            print_var( TABSTOP, def->vars.vars );
            print_defs( def->vars.next );
            break;
        case DEFFUNC:
            print_func( TABSTOP, def->funcs.funcs );
            print_defs( def->funcs.next );
            break;
    }
}
 
void print_tree() {
    printf("%p\n", GLOBAL_TREE);
    print_defs(GLOBAL_TREE);
}
