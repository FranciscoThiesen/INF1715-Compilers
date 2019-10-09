//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include "ast.h"
#include "semantic.h"
#include <stdio.h>
#define TABSTOP 4

static void print_stat( int n_spaces, Stat *stat);
static void print_exp( int n_spaces, Exp *exp );

static void print_spaces( int n, int line_skip ) {
    if( line_skip ) printf("\n");
    for(int i = 0; i < n; ++i) {
        printf(" ");
    }
}

static void print_native_type( Native_types type ) {
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

static void print_type( Type* type) {
    if( type != NULL ) {
        if( type->tag == SINGLE ) {
            print_native_type( type->single.type );
        }
        else {
            printf("[");
            print_type( type->seq.next );
            printf("]");
        }
    }
}

static void print_var( int n_spaces, Var *v ) {
    if( v != NULL ) {
        insert_var(v);
        print_spaces( n_spaces, 0);
        print_type( v->type );
        printf(" %s\n", v->name );
        print_var( n_spaces, v->next );
    }
}

static void print_params( Var* param ) {
    if( param == NULL ) return;
    insert_var(param);
    printf("%s ", param->name );
    print_type( param->type );
    if( param->next != NULL ) {
        printf(", ");
        print_params( param->next );
    }
}

static void print_expvarid( Exp *exp_var ) {
    Var *v;

    v = get_var(exp_var->var.name);
    if (!v) {
        fprintf(stderr, "undefined symbol %s\n", exp_var->var.name);
        exit(-1);
    }

    exp_var->var.def = v;

    printf("SIMPLEVAR = %s", exp_var->var.name );
}

static void print_explist( int n_spaces, Exp_list *elist ) {
    if ( !elist )
        return;

    print_exp(n_spaces, elist->exp); 
    puts("");
    print_explist(n_spaces, elist->next); 
}

static void print_call( int n_spaces, Exp *exp ) {

    Func *f;

    printf("CALL %s", exp->call.name);
    f = get_func(exp->call.name);
    if (!f) {
        fprintf(stderr, "undefined function %s\n", exp->call.name);
        exit(-1);
    }

    exp->call.funcdef = f;
    print_spaces( n_spaces, 1);
    printf("ARGS {\n");
    print_explist( n_spaces + TABSTOP, exp->call.explist );
    print_spaces( n_spaces, 1);
    printf("}\n");
}

static void print_new( int n_spaces, Type *type, Exp *exp) {
    printf("NEW {\n");
    print_spaces( n_spaces + TABSTOP, 0);
    printf("TYPE ");
    print_type( type );
    printf(" [\n");
    print_exp( n_spaces + TABSTOP, exp );
    print_spaces( n_spaces + TABSTOP, 1);
    printf("]\n");
    print_spaces ( n_spaces, 1);
    printf("}");
}

static void print_as( int n_spaces, Exp *exp, Type *type) {
    printf("AS {");
    print_spaces( n_spaces + TABSTOP, 1);
    printf("TYPE ");
    print_type( type );
    printf("\n");
    print_exp( n_spaces + TABSTOP, exp );
    print_spaces ( n_spaces, 1);
    printf("}");
}

static void print_expatt( int n_spaces, Exp *e1, Exp *e2) {
    printf("ASSIGN {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    printf("\n");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_or( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("OR {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_and( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("AND {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_not( int n_spaces, Exp *e1 ) {
    printf("NOT {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_eq( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("EQUALS {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_neq( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("NOT EQUAL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts(""); 
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_geq( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("GREATER OR EQUAL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}


static void print_leq( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("LESS OR EQUAL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts(""); 
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}


static void print_less( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("LESS {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_greater( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("GREATER {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_sum( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("SUM {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_sub( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("SUB {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_mul( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("MUL {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_div( int n_spaces, Exp *e1, Exp *e2 ) {
    printf("DIV {\n" );
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_expvar( int n_spaces, Exp *e1, Exp *e2) {
    printf("NOTSOSIMPLEVAR {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    puts("");
    print_exp( n_spaces + TABSTOP, e2 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_minus( int n_spaces, Exp *e1 ) {
    printf("MINUS {\n");
    print_exp( n_spaces + TABSTOP, e1 );
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_exp( int n_spaces, Exp *exp ) {
    if( exp != NULL ) {
        print_spaces( n_spaces, 0);
        switch( exp->tag ) {
            case VAR:
                print_expvar( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case VARID:
                print_expvarid( exp );
                break;
            case CALLEXP:
                print_call( n_spaces, exp );
                break;
            case AS:
                print_as (n_spaces, exp->as.exp, exp->as.type);
                break;
            case NEW:
                print_new( n_spaces, exp->new.type, exp->new.exp );
                break;
            case SUM:
                print_sum( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case SUB:
                print_sub( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case MINUS:
                print_minus( n_spaces, exp->unary.exp );
                break;
            case MUL:
                print_mul( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case DIV:
                print_div( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case EXPINT:
                print_type( exp->expint.type );
                printf(" %d", exp->expint.i );
                break;
            case EXPFLOAT:
                print_type( exp->expfloat.type );
                printf(" %lf", exp->expfloat.d );
                break;
            case EXPCHAR:
                print_type( exp->expchar.type );
                printf(" %c", exp->expchar.c );
                break;
            case EXPSTR:
                print_type( exp->expstr.type );
                printf(" %s", exp->expstr.str );
                break;
            case EXPBOOL:
                print_type( exp->expbool.type );
                printf( exp->expbool.b ? " TRUE" : " FALSE" );
                break;
            case EXPATT:
                print_expatt ( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case EQ:
                print_eq( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case NEQ:
                print_neq( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case LEQ:
                print_leq( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case GEQ:
                print_geq( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case L:
                print_less( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case G:
                print_greater( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case NOT:
                print_not( n_spaces, exp->unary.exp );
                break;
            case AND:
                print_and( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            case OR:
                print_or ( n_spaces, exp->binary.e1, exp->binary.e2 );
                break;
            default:
                fprintf(stderr, "Unknown expression %d!\n", exp->tag);
                exit(-1);
        }
    }
}

static void print_if( int n_spaces, Exp *exp, Stat *stat ) {
    print_spaces(n_spaces, 0);
    printf("IF (\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf(")");
    print_spaces( n_spaces, 1);
    print_stat( n_spaces, stat );
}

static void print_else( int n_spaces, Stat *stat ) {
    printf(" ELSE\n");
    if (stat) print_spaces( n_spaces, 0);
    print_stat( n_spaces, stat );
}


static void print_ifelse ( int n_spaces, Exp *exp, Stat *stat, Stat *stat2 ) {
    print_if( n_spaces, exp, stat );
    print_else( n_spaces, stat2 );
    print_spaces( n_spaces, 0);
}

static void print_ret (int n_spaces) {
    print_spaces( n_spaces, 0 );
    printf("RET\n");
}

static void print_retexp( int n_spaces, Exp *exp ) {
    print_spaces( n_spaces, 0 );
    printf("RETEXP {\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_while ( int n_spaces, Exp *exp, Stat *stat ) {
    print_spaces( n_spaces, 0 );
    printf("WHILE (\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf(")\n");
    if(stat) print_spaces( n_spaces, 0);
    print_stat( n_spaces, stat );
}

static void print_print( int n_spaces, Exp *exp ) {
    print_spaces( n_spaces, 0 );
    printf("PRINT {\n");
    print_exp( n_spaces + TABSTOP, exp);
    print_spaces( n_spaces, 1);
    printf("}");
}

static void print_cmd( int n_spaces, Cmd *cmd ) {
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

static void print_stat( int n_spaces, Stat *stat ) {
    if (!stat) {
        return;
    }

    enter_scope();
    printf("{\n");
    print_var( n_spaces + TABSTOP, stat->vars);
    print_cmd( n_spaces + TABSTOP, stat->cmds);
    print_spaces( n_spaces, 1);
    printf("}");
    leave_scope();
}

static void print_func( int n_spaces, Func* f ) {
    if( f != NULL ) {
        insert_func(f);
        print_spaces( n_spaces, 1);
        printf("FUNC< ");
        print_type( f->type );
        printf(" > ");
        printf(" %s, ", f->name );
        enter_scope();
        printf("PARAMS = {");
        print_params( f->param );
        printf("}\n");
        if( f->stat ) {
            print_spaces( n_spaces, 0);
            print_stat( n_spaces, f->stat );
        }
        leave_scope();
        if( f->next ) {
            print_spaces( n_spaces, 1);
            print_func( n_spaces, f->next );
        }
    }
}

void print_defs(Def *def) {
    if (!def) {
        return;
    }

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
    print_defs(GLOBAL_TREE);
}
