#include "ast.h"
#include <stdio.h>
#define TABSTOP 4

static void print_stat( int n_spaces, struct stat *stat);
static void print_exp( int n_spaces, union exps *exp );

static void print_spaces( int n ) {
    printf("\n");
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
        //print_spaces( n_spaces );
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
        print_spaces( n_spaces );
        print_type( 0, v->type );
        printf(" %s", v->name );
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

static void print_expvar( int n_spaces, union exps *exp_var ) {
    print_spaces( n_spaces );
    printf("SIMPLEVAR = %s\n", exp_var->var.name );
}

static void print_call( int n_spaces, union exps *exp ) {

    printf("CALL %s", exp->call.name);
    print_spaces( n_spaces );
    printf("ARGS {");
    print_exp( n_spaces + TABSTOP, exp->call.listexp );
    print_spaces( n_spaces );
    printf("}\n");
}

static void print_new( int n_spaces, union type *type, union exps *exp) {
    printf("NEW {");
    print_spaces( n_spaces + TABSTOP );
    printf("TYPE ");
    print_type( 0, type );
    printf(" [\n");
    print_exp( n_spaces + TABSTOP, exp );
    print_spaces( n_spaces + TABSTOP );
    printf("]\n");
    print_spaces ( n_spaces );
    printf("}\n");
}

static void print_expatt ( int n_spaces, union exps *e1, union exps *e2) {
    printf("ASSIGN {");
    print_exp( n_spaces + TABSTOP, e1 );
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces );
    printf("}");
}

static void print_exp( int n_spaces, union exps *exp ) {
    if( exp != NULL ) {
        print_spaces( n_spaces );
        switch( exp->tag ) {
            case VAR:
                print_expvar( n_spaces, exp );
                print_exp( n_spaces, exp->var.next );
                break;
            case CALLEXP:
                print_call( n_spaces, exp );
                print_exp( n_spaces, exp->call.next );
                break;
            case AS:
                break;
            case NEW:
                print_new( n_spaces, exp->new.type, exp->new.exp );
                print_exp( n_spaces, exp->call.next );
                break;
            case SUM:
                break;
            case SUB:
                break;
            case EXPINT:
                break;
            case EXPFLOAT:
                break;
            case EXPCHAR:
                break;
            case EXPSTR:
                break;
            case EXPBOOL:
                break;
            case EXPATT:
                print_expatt ( n_spaces, exp->binary.e1, exp->binary.e2 );
                print_exp( n_spaces, exp->binary.next );
                break;
            case EQ:
                break;
            case NEQ:
                break;
            case LEQ:
                break;
            case GEQ:
                break;
            case L:
                break;
            case G:
                break;
            case NOT:
                break;
            case AND:
                break;
            case OR:
                break;
            default:
                fprintf(stderr, "Unknown expression %d!\n", exp->tag);
                exit(-1);
        }
    }
}

static void print_if( int n_spaces, union exps *exp, struct stat *stat ) {
    printf("IF");
    print_exp( n_spaces, exp);
    print_spaces( n_spaces );
    print_stat( n_spaces, stat );
}

static void print_ifelse ( int n_spaces, union exps *exp, struct stat *stat, struct stat *stat2 ) {
    printf("IFELSE");
    print_exp( n_spaces, exp);
    print_spaces( n_spaces );
    print_stat( n_spaces, stat );
    print_spaces( n_spaces );
    print_stat( n_spaces, stat2 );
}

static void print_ret (int n_spaces) {
    printf("RET");
}

static void print_retexp( int n_spaces, union exps *exp ) {
    printf("RETEXP");
    print_exp( n_spaces, exp);
}

static void print_while ( int n_spaces, union exps *exp, struct stat *stat ) {
    printf("WHILE");
    print_exp( n_spaces, exp);
    print_spaces( n_spaces );
    print_stat( n_spaces, stat );
}

static void print_print( int n_spaces, union exps *exp ) {
    printf("PRINT {");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces );
    printf("}");
}

static void print_cmd( int n_spaces, union cmd *cmd ) {
    if (!cmd)
        return;

    print_spaces( n_spaces );
    switch ( cmd->tag ) {
        case IF:
            print_if( n_spaces, cmd->cmd_if.exp, cmd->cmd_if.stat);
            print_cmd( n_spaces, cmd->cmd_if.next);
            break;
        case IFELSE:
            print_ifelse( n_spaces, cmd->cmd_ifelse.exp, cmd->cmd_ifelse.stat, cmd->cmd_ifelse.stat2 );
            print_cmd( n_spaces, cmd->cmd_if.next );
            break;
        case RET:
            print_ret( n_spaces );
            print_cmd( n_spaces, cmd->cmd_ret.next );
            break;
        case RETEXP:
            print_retexp( n_spaces, cmd->cmd_ret_exp.exp);
            print_cmd( n_spaces, cmd->cmd_ret_exp.next );
            break;
        case WHILE:
            print_while( n_spaces, cmd->cmd_while.exp, cmd->cmd_while.stat);
            break;
        case PRINT:
            print_print( n_spaces, cmd->print.exp);
            print_cmd( n_spaces, cmd->print.next );
            break;
        case CALLCMD:
            print_exp( n_spaces, cmd->call.call );
            print_cmd( n_spaces, cmd->call.next );
            break;
        case ATTCMD:
            print_exp( n_spaces, cmd->att.att );
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
    print_spaces( n_spaces);
    printf("}\n");
}

static void print_func( int n_spaces, struct func* f ) {
    if( f != NULL ) {
        print_spaces( n_spaces );
        printf("FUNC< ");
        print_type( 0, f->type );
        printf(" > ");
        printf(" %s, ", f->name );
        printf("PARAMS = {");
        print_params( f->param );
        printf("}\n");
        print_spaces( n_spaces );
        print_stat( n_spaces, f->stat );
        print_spaces( n_spaces );
        print_func( n_spaces, f->next );
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
