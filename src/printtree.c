//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include "ast.h"
#include "semantic.h"
#include "printtree.h"
#include <stdio.h>
#define TABSTOP 4

#define CAST(father, ecast, exptype, eorig, type)   \
    ecast = asexp(eorig, type);     \
    father->exptype.eorig = ecast;

static int compare_type(Type *t1, Type *t2) {
    if(t1->tag != t2->tag) return 0;
    if(t1->tag == SEQ) return compare_type(t1->seq.next, t2->seq.next );
    return ( t1->single.type == t2->single.type );
}

static int is_float(Type *t) {
    if(t->tag == SINGLE) return (t->single.type == FLOAT);
    return is_float(t->seq.next);
}

static int is_int(Type  *t) {
    if(t->tag == SINGLE) return (t->single.type == INT);
    return is_int(t->seq.next);
}

static int is_char(Type *t) {
    if(t->tag == SINGLE) return (t->single.type == CHAR);
    return is_char(t->seq.next);
}

static int is_bool(Type *t) {
    if(t->tag == SINGLE) return (t->single.type == BOOL);
    return is_bool(t->seq.next);
}

static int is_array(Type *t) { return (t->tag == SEQ); }

static int is_numeral(Type *t) {
    return ((is_int(t) || is_float(t) || is_char(t)) && !is_array(t));
}

static Type *get_exp( Exp *exp );

static void print_stat( Stat *stat );

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

static void print_var( Var *v ) {
    if( v != NULL ) {
        insert_var(v);
        print_type( v->type );
        printf(" %s\n", v->name );
        print_var( v->next );
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

static Type *get_expvarid( Exp *exp_var ) {
    Var *v;
    v = get_var(exp_var->var.name);
    if (!v) {
        fprintf(stderr, "undefined symbol %s\n", exp_var->var.name);
        exit(-1);
    }

    return v->type;
}

static void check_explist( char *name,  Var *param_list, Exp_list *arg_list) {
    if( param_list == NULL && arg_list == NULL) return;
    if( param_list == NULL || arg_list == NULL) {
        if( param_list == NULL ) fprintf(stderr, "Too many arguments to function %s\n", name);
        else fprintf(stderr, "Too few arguments to function %s\n", name);
        exit(-1);
    }
   
    Type *t1, *t2;
    t1 = param_list->type;
    t2 = get_exp(arg_list->exp);
    Exp *eaux;
    if( is_numeral(t1) && is_numeral(t2) ) {
        if( !compare_type(t1, t2) ) {
            eaux = asexp( arg_list->exp, t1 );
            arg_list->exp = eaux;
        }
    }
    else if( (is_bool(t1) && !is_array(t1)) && is_numeral(t2) ) {
        if( is_float(t2) ) {
            fprintf(stderr, "Float cannot be cast down to bool on %s function\n", name);
            exit(-1);
        }
        else {
            eaux = asexp( arg_list->exp, t1 );
            arg_list->exp = eaux;
        }
    }
    else if( is_numeral(t1) && (is_bool(t2) && !is_array(t2) ) ) {
        if( is_float(t1) ) {
            fprintf(stderr, "Bool cannot be cast to float on %s function call\n", name);
            exit(-1);
        }
        else {
            eaux = asexp( arg_list->exp, t1);
            arg_list->exp = eaux;
        }
    }
    else if( !compare_type(t1, t2) ) {
        fprintf(stderr, "Incompatible argument type passed to function %s\n", name);
        exit(-1);
    }
   
    check_explist( name, param_list->next, arg_list->next );
}

static Type *get_call( Exp *exp ) {

    Func *f;

    printf("CALL %s", exp->call.name);
    f = get_func(exp->call.name);
    if (!f) {
        fprintf(stderr, "undefined function %s\n", exp->call.name);
        exit(-1);
    }

    exp->call.funcdef = f;
    printf("ARGS {\n");
    check_explist( exp->call.name, f->param, exp->call.explist );
    printf("}\n");

    return f->type;
}

static Type *get_new( Type *type, Exp *exp) {
    Type *texp;

    texp = get_exp( exp );
    if (!is_int(texp)) {
        fprintf(stderr, "expression in new must be an integer\n");
        exit(-1);
    }

    return type;
}

static Type *get_as( Exp *exp, Type *type) {
    Type *texp;

    texp = get_exp( exp );
    if (is_array( texp )) {
        fprintf(stderr, "error: cast of array not allowed\n");
        exit(-1);
    }

    if (is_array( type )) {
        fprintf(stderr, "error: cast to array not allowed\n");
        exit(-1);
    }

    if ((is_bool(texp) && is_float(type))) {
        fprintf(stderr, "error: cannot cast bool to float\n");
        exit(-1);
    }

    if ((is_float(texp) && is_bool(type))) {
        fprintf(stderr, "error: cannot cast float to bool\n");
        exit(-1);
    }

    return type;
}

static Type *get_expatt( Exp *father, Exp *e1, Exp *e2) {
    Type *t1, *t2;
    Exp *eaux;

    t1 = get_exp( e1 );
    t2 = get_exp( e2 );

    if (!is_array(t1) && !is_array(t2)) {
        if (compare_type(t1, t2)) {
            return t1;
        }

        if ((is_bool(t1) && is_float(t2))) {
            fprintf(stderr, "error: cannot assign float to bool");
            exit(-1);
        }
        if ((is_float(t1) && is_bool(t2))) {
            fprintf(stderr, "error: cannot assign bool to float");
            exit(-1);
        }
        CAST(father, eaux, binary, e2, t1);
        return t1;
    }

    //Same type array atribution only valid if left side is new expression
    if (!compare_type(t1, t2)) {
        fprintf(stderr, "assignment with expression with different"
                        "array type\n");
        exit(-1);
    }
   
    return t1;
}

static Type *get_bin_logical( Exp *father, Exp *e1, Exp *e2 ) {
    Type *t1, *t2;
    Type *tbool;
    Exp *eaux, *eaux2;

    t1 = get_exp( e1 );
    t2 = get_exp( e2 );

    tbool = newtype(BOOL);
    if (!is_bool(t1) || is_array(t1)) {
        CAST(father, eaux, binary, e1, tbool);
    }
    if (!is_bool(t2) || is_array(t2)) {
        CAST(father, eaux2, binary, e2, tbool);
    } 
    return tbool;
}

static Type *get_not( Exp *father, Exp *e1 ) {
    Type *t1;
    Type *tbool;
    Exp *eaux;

    t1 = get_exp( e1 );

    tbool = newtype(BOOL);
    if (!is_bool(t1) || is_array(t1)) {
        CAST(father, eaux, binary, e1, tbool);
    }
    return tbool;
}

static Type *get_equality_exp( Exp *father, Exp *e1, Exp *e2 ) {
    Type *t1, *t2;
    Exp *eaux;
    Type *tbool;

    t1 = get_exp( e1 );
    t2 = get_exp( e2 );

    tbool = newtype(BOOL);
    if (compare_type(t1, t2))
        return tbool;

    if (is_array(t1) && is_array(t2)) {
        fprintf(stderr, "error: comparing different array types");
        exit(-1);
    }

    if (is_array(t1) || is_array(t2)) {
        fprintf(stderr, "error: attempt to compare array "
                        "with non array type\n");
        exit(-1);
    }

    if ((is_float(t1) && is_bool(t2)) || (is_bool(t1) && is_float(t2))) {
        fprintf(stderr, "error: attempting to compare float with bool\n");
        exit(-1);
    }

    if (is_float(t1) && (is_numeral(t2) || is_bool(t2)))  {
        CAST(father, eaux, binary, e2, t1);
        return tbool;
    }

    if (is_float(t2) && (is_numeral(t1) || is_bool(t1)))  {
        CAST(father, eaux, binary, e1, t2);
        return tbool;
    }

    if (is_int(t1) && is_char(t2))  {
        CAST(father, eaux, binary, e2, t1);
        return tbool;
    }

    if (is_int(t2) && is_char(t1))  {
        CAST(father, eaux, binary, e1, t2);
        return tbool;
    }

    if (is_bool(t1) && !is_bool(t2)) {
        CAST(father, eaux, binary, e1, t2);
        return tbool;
    }

    if (is_bool(t2) && !is_bool(t1)) {
        CAST(father, eaux, binary, e2, t1);
        return tbool;
    }

    return tbool;
}

static Type *get_compare_exp( Exp *father, Exp *e1, Exp *e2 ) {
    Type *t1, *t2;
    Exp *eaux;
    Type *tbool;

    t1 = get_exp( e1 );
    t2 = get_exp( e2 );

    tbool = newtype(BOOL);
    if (is_array(t1) || is_array(t2)) {
        fprintf(stderr, "error: attempt to compare array values\n");
        exit(-1);
    }

    if (is_bool(t1) || is_bool(t2)) {
        fprintf(stderr, "error: attempt to compare boolean values\n");
        exit(-1);
    }

    if (is_float(t1) && (is_numeral(t2) || is_bool(t2)))  {
        CAST(father, eaux, binary, e2, t1);
        return tbool;
    }

    if (is_float(t2) && (is_numeral(t1) || is_bool(t1)))  {
        CAST(father, eaux, binary, e1, t2);
        return tbool;
    }

    if (is_int(t1) && is_char(t2))  {
        CAST(father, eaux, binary, e2, t1);
        return tbool;
    }

    if (is_int(t2) && is_char(t1))  {
        CAST(father, eaux, binary, e1, t2);
        return tbool;
    }

    return tbool;
}

static Type *get_arit_type(Exp *father, Exp *e1, Exp *e2) {
    Type *t1, *t2;
    Exp *eaux;
    Type *tint;

    t1 = get_exp( e1 );
    t2 = get_exp( e2 );

    tint = newtype(INT);
    if (is_char(t1)) {
        CAST(father, eaux, binary, e1, tint)
    }
    if (is_char(t2)) {
        CAST(father, eaux, binary, e2, tint)
    }
    if ((!is_numeral(t1) && !is_bool(t1)) || (!is_bool(t2) && !is_numeral(t2))) {
        fprintf(stderr, "error: invalid operand to binary ");
        switch (father->tag) {
            case SUM:
                fprintf(stderr, "sum\n");
                break;
            case SUB:
                fprintf(stderr, "sub\n");
                break;
            case MUL:
                fprintf(stderr, "mul\n");
                break;
            case DIV:
                fprintf(stderr, "div\n");
                break;
            default:
                //Never happens
                break;
        }
        exit(-1);
    }

    if( compare_type(t1, t2) ) return t1;

    // (int, int) -> int, (float, float) -> float   
    // cast t1 to float
    printf("}");

    if(is_int(t1)) {
        CAST(father, eaux, binary, e1, t2);
            return t2;
    } else {
        CAST(father, eaux, binary, e2, t1)
            return t1;
    }

}

static Type *get_expvar( Exp *e1, Exp *e2) {
    Type *t1, *t2;

    printf("NOTSOSIMPLEVAR {\n");
    t1 = get_exp( e1 );
    if (!is_array( t1 )) {
        fprintf(stderr, "error: subscripted value is not an array\n");
        exit(-1);
    }

    puts("");
    t2 = get_exp( e2 );
    if (!is_int( t2 ) || is_array(t2)) {
        fprintf(stderr, "error: array index is not an integer\n");
        exit(-1);
    }

    printf("}");
    return t1->seq.next;
}

static Type *get_minus( Exp *father, Exp *e1 ) {
    Type *t1;
    Exp *eaux;

    printf("MINUS {\n");
    t1 = get_exp( e1 );
    if ((!is_int(t1) && !is_float(t1) && !is_char(t1)) || is_array(t1)) {
        fprintf(stderr, "error: wrong type argument to unary minus\n");
        exit(-1);
    }

    if (is_char(t1)) {
        t1 = newtype(INT);
        CAST(father, eaux, binary, e1, t1)
    }

    return t1;
}

static Type *get_exp( Exp *exp ) {
    if( exp != NULL ) {
        switch( exp->tag ) {
            case VAR:
                return get_expvar( exp->binary.e1, exp->binary.e2 );
            case VARID:
                return get_expvarid( exp );
                /*
                   if( exp->var.next ) puts("");
                   get_exp( exp->var.next );
                   break;
                 */
            case CALLEXP:
                return get_call( exp );
            case AS:
                return get_as (exp->as.exp, exp->as.type);
            case NEW:
                return get_new( exp->new.type, exp->new.exp );
            case SUM:
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case SUB:
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case MINUS:
                return get_minus(exp, exp->unary.exp );
            case MUL:
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case DIV:
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case EXPINT:
                return exp->expint.type;
            case EXPFLOAT:
                return exp->expfloat.type;
            case EXPCHAR:
                return exp->expchar.type;
            case EXPSTR:
                return exp->expstr.type;
            case EXPBOOL:
                return exp->expbool.type;
            case EXPATT:
                return get_expatt( exp, exp->binary.e1, exp->binary.e2 );
            case EQ:
                return get_equality_exp( exp, exp->binary.e1, exp->binary.e2 );
            case NEQ:
                return get_equality_exp( exp, exp->binary.e1, exp->binary.e2 );
            case LEQ:
                return get_compare_exp( exp, exp->binary.e1, exp->binary.e2 );
            case GEQ:
                return get_compare_exp ( exp, exp->binary.e1, exp->binary.e2 );
                break;
            case L:
                return get_compare_exp( exp, exp->binary.e1, exp->binary.e2 );
            case G:
                return get_compare_exp( exp, exp->binary.e1, exp->binary.e2 );
            case NOT:
                return get_not( exp, exp->unary.exp );
            case AND:
                return get_bin_logical( exp, exp->binary.e1, exp->binary.e2 );
            case OR:
                return get_bin_logical( exp, exp->binary.e1, exp->binary.e2 );
            default:
                fprintf(stderr, "Unknown expression %d!\n", exp->tag);
                exit(-1);
        }
    }

    return NULL;
}

static void print_if( Cmd *father, Exp *exp, Stat *stat ) {
    printf("IF (\n");
    Type *t1;
    Exp *eaux;
    
    t1 = get_exp( exp);
    if( is_array(t1) ) {
        fprintf(stderr, "While condition cannot be an array\n");
        exit(-1);
    }
    else {
        if( is_numeral(t1) && !is_float(t1) ) {
            Type *tbool;

            tbool = newtype(BOOL);
            eaux = asexp( exp, tbool );
            father->cmd_if.exp = eaux;
        }
        else {
            fprintf(stderr, "While condition cannot be converted to a boolean type\n");
            exit(-1);
        }
    }
    printf(")");
    print_stat( stat );
}

static void print_else( Stat *stat ) {
    printf(" ELSE\n");
    if (stat)     print_stat( stat );
}


static void print_ifelse ( Cmd *father, Exp *exp, Stat *stat, Stat *stat2 ) {
    print_if( father, exp, stat );
    print_else( stat2 );
}

static void print_ret () {
    printf("RET\n");
}

static void get_retexp( Cmd *father, Exp *exp ) {
    Type *t1, *t2;
    char *name;
    name = global_state->cur_func_name;
    t1 = global_state->cur_func_type;
    t2 = get_exp( exp );
    if( compare_type(t1, t2)) return;
    // hora do show
    Exp *eaux;
    if( is_numeral(t1) && is_numeral(t2) ) {
        if( !compare_type(t1, t2) ) {
            // quero que exp tenha o tipo t1
            eaux = asexp( exp, t1 );
            father->cmd_ret_exp.exp = eaux;
        }
    }
    else if( (is_bool(t1) && !is_array(t1)) && is_numeral(t2) ) {
        if( is_float(t2) ) {
            fprintf(stderr, "Float is not compatible with %s return type\n", name);
            exit(-1);
        }
        else {
            eaux = asexp( exp, t1 );
            father->cmd_ret_exp.exp = eaux;
        }
    }
    else if( is_numeral(t1) && (is_bool(t2) && !is_array(t2) ) ) {
        if( is_float(t1) ) {
            fprintf(stderr, "Bool is not compatible with %s return type\n", name);
            exit(-1);
        }
        else {
            eaux = asexp( exp, t1);
            father->cmd_ret_exp.exp = eaux;
        }
    }
    else if( !compare_type(t1, t2) ) {
        fprintf(stderr, "Wrong return type for function %s\n", name);
        exit(-1);
    }

    printf("RETEXP {\n");
    get_exp(exp);
    printf("}");
}

static void print_while ( Cmd *father, Exp *exp, Stat *stat ) {
    printf("WHILE (\n");
    Type *t1;
    
    t1 = get_exp(exp);
    // certificar de que esta single bool
    Exp *eaux;
    if( is_array(t1) ) {
        // nao pode dar certo
        fprintf(stderr, "While condition cannot be an array" );
        exit(-1);
    }
    else {
        if( is_numeral(t1) && !is_float(t1) ) {
            Type *tbool;
            
            tbool = newtype(BOOL);
            eaux = asexp( exp, tbool );
            father->cmd_while.exp = eaux;
        }
        else {
            fprintf(stderr, "While condition cannot be converted to boolean type\n");
            exit(-1);
        }
    }
    printf(")\n");
    if(stat)     print_stat( stat );
}

static void print_print( Exp *exp ) {
    printf("PRINT {\n");
    get_exp( exp);
    printf("}");
}

static void print_cmd( Cmd *cmd ) {
    if (!cmd)
        return;
    switch ( cmd->tag ) {
        case IF:
            print_if( cmd, cmd->cmd_if.exp, cmd->cmd_if.stat);
            if( cmd->cmd_if.next ) printf("\n");
            print_cmd( cmd->cmd_if.next);
            break;
        case IFELSE:
            print_ifelse( cmd, cmd->cmd_ifelse.exp, cmd->cmd_ifelse.stat, cmd->cmd_ifelse.stat2 );
            if( cmd->cmd_ifelse.next) printf("\n");
            print_cmd( cmd->cmd_ifelse.next );
            break;
        case RET:
            print_ret();
            if( cmd->cmd_ret.next) printf("\n");
            print_cmd( cmd->cmd_ret.next );
            break;
        case RETEXP:
            get_retexp( cmd, cmd->cmd_ret_exp.exp);
            if( cmd->cmd_ret_exp.next) printf("\n");
            print_cmd( cmd->cmd_ret_exp.next );
            break;
        case WHILE:
            print_while( cmd, cmd->cmd_while.exp, cmd->cmd_while.stat);
            if( cmd->cmd_while.next) printf("\n");
            print_cmd( cmd->cmd_while.next );
            break;
        case PRINT:
            print_print( cmd->print.exp);
            if( cmd->print.next) printf("\n");
            print_cmd( cmd->print.next );
            break;
        case CALLCMD:
            get_exp( cmd->call.call );
            if( cmd->call.next ) printf("\n");
            print_cmd( cmd->call.next );
            break;
        case ATTCMD:
            get_exp( cmd->att.att );
            if( cmd->att.next ) printf("\n");
            print_cmd( cmd->att.next );
            break;
        case STAT:
            print_cmd( cmd->stat.next);
            break;
        default:
            fprintf(stderr, "Unknown command!\n");
            exit(-1);
    }
}

static void print_stat( Stat *stat ) {
    if (!stat) {
        return;
    }

    enter_scope();
    printf("{\n");
    print_var( stat->vars);
    print_cmd( stat->cmds);
    printf("}");
    leave_scope();
}

static void print_func( Func* f ) {
    if( f != NULL ) {
        insert_func(f);
        global_state->cur_func_type = f->type; 
        global_state->cur_func_name = f->name;
        printf("FUNC< ");
        print_type( f->type );
        printf(" > ");
        printf(" %s, ", f->name );
        enter_scope();
        printf("PARAMS = {");
        print_params( f->param );
        printf("}\n");
        if( f->stat ) {
            print_stat( f->stat );
        }
        leave_scope();
        if( f->next ) {
            print_func( f->next );
        }
    }
}

void print_defs(Def *def) {
    if (!def) {
        return;
    }

    switch (def->tag) {
        case DEFVAR:
            print_var( def->vars.vars );
            print_defs( def->vars.next );
            break;
        case DEFFUNC:
            print_func( def->funcs.funcs );
            print_defs( def->funcs.next );
            break;
    }
}

void print_tree() {
    print_defs(GLOBAL_TREE);
}
