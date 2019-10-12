//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include "ast.h"
#include "symbols.h"
#include "typing.h"
#include <stdio.h>
#include <stdbool.h>
#define TABSTOP 4

#define CAST(father, ecast, exptype, eorig, type)   \
    ecast = asexp(eorig, global_state->cur_line, type);     \
    father->exptype.eorig = ecast;

static bool accepted = true;

static int compare_type(Type *t1, Type *t2) {
	if (t1 == NULL || t2 == NULL)
		return 0;

    if(t1->tag != t2->tag) return 0;
    if(t1->tag == SEQ) return compare_type(t1->seq.next, t2->seq.next );
    return ( t1->single.type == t2->single.type );
}

static int is_float(Type *t) {
	if (t == NULL)
		return 0;

    if(t->tag == SINGLE) return (t->single.type == FLOAT);
    return is_float(t->seq.next);
}

static int is_int(Type  *t) {
	if (t == NULL)
		return 0;

    if(t->tag == SINGLE) return (t->single.type == INT);
    return is_int(t->seq.next);
}

static int is_char(Type *t) {
	if (t == NULL)
		return 0;

    if(t->tag == SINGLE) return (t->single.type == CHAR);
    return is_char(t->seq.next);
}

static int is_bool(Type *t) {
	if (t == NULL)
		return 0;

    if(t->tag == SINGLE) return (t->single.type == BOOL);
    return is_bool(t->seq.next);
}

static int is_array(Type *t) { 
	if (t == NULL)
		return 0;

	return (t->tag == SEQ);
}

static int is_numeral(Type *t) {
	if (t == NULL)
		return 0;

    return ((is_int(t) || is_float(t) || is_char(t)) && !is_array(t));
}

static Type *get_exp( Exp *exp );

static void type_stat( Stat *stat );

static void type_var( Var *v ) {
    if( v != NULL ) {
        insert_var(v);
        type_var( v->next );
    }
}
static void type_params( Var* param ) {
    if( param == NULL ) return;
    insert_var(param);
    if( param->next != NULL )
        type_params( param->next );
}

static Type *get_expvarid( Exp *exp_var ) {
    Var *v;
    bool error;

    v = get_var(exp_var->var.name, &error);
    if (error) {
        fprintf(stderr, "error: expected symbol %s to be a variable or parameter in"
                " line %d\n", exp_var->var.name, global_state->cur_line);

        accepted = false;
		return NULL;
    }

    if (!v) {
        fprintf(stderr, "error: undefined symbol %s in line %d\n", exp_var->var.name,
                global_state->cur_line );

        accepted = false;
		return NULL;
    }

	return v->type;
}

static void check_explist( char *name,  Var *param_list, Exp_list *arg_list) {
    if( param_list == NULL && arg_list == NULL) return;
    if( param_list == NULL || arg_list == NULL) {
        if( param_list == NULL ) {
            fprintf(stderr, "error: too many arguments to function %s in line %d\n",
                    name, global_state->cur_line);
			return ;
		} else {
            fprintf(stderr, "error: too few arguments to function %s in line %d\n",
                    name, global_state->cur_line);
			return ;
		}
        accepted = false;
    }

    Type *t1, *t2;
    t1 = param_list->type;
    t2 = get_exp(arg_list->exp);
    Exp *eaux;
    if( is_numeral(t1) && is_numeral(t2) ) {
        if( !compare_type(t1, t2) ) {
            eaux = asexp( arg_list->exp, global_state->cur_line, t1 );
            arg_list->exp = eaux;
        }
    }
    else if( (is_bool(t1) && !is_array(t1)) && is_numeral(t2) ) {
        if( is_float(t2) ) {
            fprintf(stderr, "error: float cannot be cast down to bool on %s function"
							" in line %d\n", name, global_state->cur_line);
            accepted = false;
        }
        else {
            eaux = asexp( arg_list->exp, global_state->cur_line, t1 );
            arg_list->exp = eaux;
        }
    }
    else if( is_numeral(t1) && (is_bool(t2) && !is_array(t2) ) ) {
        if( is_float(t1) ) {
            fprintf(stderr, "error: bool cannot be cast to float on %s function call"
							" in line %d\n", name, global_state->cur_line);
            accepted = false;
        }
        else {
            eaux = asexp( arg_list->exp, global_state->cur_line, t1);
            arg_list->exp = eaux;
        }
    }
    else if( !compare_type(t1, t2) ) {
        fprintf(stderr, "error: incompatible argument type passed to function"
						" %s in line %d\n", name, global_state->cur_line);
        accepted = false;
    }

    check_explist( name, param_list->next, arg_list->next );
}

static Type *get_call( Exp *exp ) {

    Func *f;
    bool error;

    f = get_func(exp->call.name, &error);

    if (error) {
        fprintf(stderr, "error: expected symbol %s to be a function"
						" in line %d\n", exp->call.name, global_state->cur_line);
		accepted = false;

		return NULL;
	}

    if (!f) {
        fprintf(stderr, "error: undefined function %s\n", exp->call.name);
        accepted = false;

		return NULL;
    }

    exp->call.funcdef = f;
    check_explist( exp->call.name, f->param, exp->call.explist );

    return f->type;
}

static Type *get_new( Type *type, Exp *exp) {
    Type *texp;

    texp = get_exp( exp );
    if (!is_int(texp)) {
        fprintf(stderr, "error: expression inside new must be an integer in line %d\n",
							global_state->cur_line);
        accepted = false;
    }

    return type;
}

static Type *get_as( Exp *exp, Type *type) {
    Type *texp;

    texp = get_exp( exp );
    if (is_array( texp )) {
        fprintf(stderr, "error: cast of array not allowed in line %d\n",
					global_state->cur_line);
        accepted = false;
		return texp;
    }

    if (is_array( type )) {
        fprintf(stderr, "error: cast to array not allowed in line %d\n",
					global_state->cur_line);
        accepted = false;
		return type;
    }

    if ((is_bool(texp) && is_float(type))) {
        fprintf(stderr, "error: cannot cast bool to float in line %d\n",
					global_state->cur_line);
        accepted = false;
		return texp;
    }

    if ((is_float(texp) && is_bool(type))) {
        fprintf(stderr, "error: cannot cast float to bool in line %d\n",
					global_state->cur_line);
		return texp;
        accepted = false;
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
            fprintf(stderr, "error: cannot assign float to bool in line %d\n",
						global_state->cur_line);
            accepted = false;
        }
        if ((is_float(t1) && is_bool(t2))) {
            fprintf(stderr, "error: cannot assign bool to float in line %d\n",
						global_state->cur_line);
            accepted = false;
        }
        CAST(father, eaux, binary, e2, t1);
        return t1;
    }

    //Same type array atribution only valid if left side is new expression
    if (!compare_type(t1, t2)) {
        fprintf(stderr, "error: assignment with expression with different "
                "array type in line %d\n", global_state->cur_line);
        accepted = false;
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
	if (is_float(t1) || is_float(t2) || is_array(t1) || is_array(t2)) {
		fprintf(stderr, "error: invalid operand in logical");
		switch (father->tag) {
			case OR:
				fprintf(stderr, " OR operation in line %d\n", global_state->cur_line);
				break;
			case AND:
				fprintf(stderr, " AND operation in line %d\n", global_state->cur_line);
				break;
			default:
				break;
		}
		accepted = false;
		return NULL;
	}
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
    if (compare_type(t1, t2)) {
        return tbool;
    }

    if (is_array(t1) && is_array(t2)) {
        fprintf(stderr, "error: comparing different array types in line %d\n",
					global_state->cur_line);
        accepted = false;
    }

    if (is_array(t1) || is_array(t2)) {
        fprintf(stderr, "error: attempt to compare array "
                "with non array type in line %d\n", global_state->cur_line);
        accepted = false;
    }

    if ((is_float(t1) && is_bool(t2)) || (is_bool(t1) && is_float(t2))) {
        fprintf(stderr, "error: attempting to compare float with bool" 
						" in line %d\n", global_state->cur_line);
        accepted = false;
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
        fprintf(stderr, "error: attempt to compare array values in line %d\n",
					global_state->cur_line);
        accepted = false;
    }

    if (is_bool(t1) || is_bool(t2)) {
        fprintf(stderr, "error: attempt to compare boolean values in line %d\n",
					global_state->cur_line);
        accepted = false;
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
    if (!is_numeral(t1) || !is_numeral(t2)) {
        fprintf(stderr, "error: invalid operand to binary ");
        switch (father->tag) {
            case SUM:
                fprintf(stderr, "sum in line %d\n", global_state->cur_line);
                break;
            case SUB:
                fprintf(stderr, "sub in line %d\n", global_state->cur_line);
                break;
            case MUL:
                fprintf(stderr, "mul in line %d\n", global_state->cur_line);
                break;
            case DIV:
                fprintf(stderr, "div in line %d\n", global_state->cur_line);
                break;
            default:
                //Never happens
                break;
        }
        accepted = false;
    }
    if( compare_type(t1, t2) ) return t1;

    // (int, int) -> int, (float, float) -> float   
    // cast t1 to float

    if(is_int(t1)) {
        CAST(father, eaux, binary, e1, t2)
        return t2;
    } else {
        CAST(father, eaux, binary, e2, t1)
        return t1;
    }

}

static Type *get_expvar( Exp *e1, Exp *e2) {
    Type *t1, *t2;

    t1 = get_exp( e1 );
    if (!is_array( t1 )) {
        fprintf(stderr, "error: subscripted value is not an array in line %d\n",
					global_state->cur_line);
        accepted = false;
    }

    puts("");
    t2 = get_exp( e2 );
    if (!is_int( t2 ) || is_array(t2)) {
        fprintf(stderr, "error: array index is not an integer in line %d\n",
					global_state->cur_line);
        accepted = false;
    }

    return t1->seq.next;
}

static Type *get_minus( Exp *father, Exp *e1 ) {
    Type *t1;
    Exp *eaux;

    global_state->cur_line = father->unary.line;
    t1 = get_exp( e1 );
    if ((!is_int(t1) && !is_float(t1) && !is_char(t1)) || is_array(t1)) {
        fprintf(stderr, "error: wrong type argument to unary minus"
					" in line %d\n", global_state->cur_line);
        accepted = false;
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
                global_state->cur_line = exp->as.line;
                return get_as (exp->as.exp, exp->as.type);
            case NEW:
                global_state->cur_line = exp->new.line;
                return get_new( exp->new.type, exp->new.exp );
            case SUM:
                global_state->cur_line = exp->binary.line;
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case SUB:
                global_state->cur_line = exp->binary.line;
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case MINUS:
                global_state->cur_line = exp->binary.line;
                return get_minus(exp, exp->unary.exp );
            case MUL:
                global_state->cur_line = exp->binary.line;
                return get_arit_type(exp, exp->binary.e1, exp->binary.e2) ;
            case DIV:
                global_state->cur_line = exp->binary.line;
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
                global_state->cur_line = exp->binary.line;
                return get_expatt( exp, exp->binary.e1, exp->binary.e2 );
            case EQ:
                global_state->cur_line = exp->binary.line;
                return get_equality_exp( exp, exp->binary.e1, exp->binary.e2 );
            case NEQ:
                global_state->cur_line = exp->binary.line;
                return get_equality_exp( exp, exp->binary.e1, exp->binary.e2 );
            case LEQ:
                global_state->cur_line = exp->binary.line;
                return get_compare_exp( exp, exp->binary.e1, exp->binary.e2 );
            case GEQ:
                global_state->cur_line = exp->binary.line;
                return get_compare_exp ( exp, exp->binary.e1, exp->binary.e2 );
                break;
            case L:
                global_state->cur_line = exp->binary.line;
                return get_compare_exp( exp, exp->binary.e1, exp->binary.e2 );
            case G:
                global_state->cur_line = exp->binary.line;
                return get_compare_exp( exp, exp->binary.e1, exp->binary.e2 );
            case NOT:
                return get_not( exp, exp->unary.exp );
            case AND:
                global_state->cur_line = exp->binary.line;
                return get_bin_logical( exp, exp->binary.e1, exp->binary.e2 );
            case OR:
                global_state->cur_line = exp->binary.line;
                return get_bin_logical( exp, exp->binary.e1, exp->binary.e2 );
            default:
                fprintf(stderr, "Unknown expression %d!\n", exp->tag);
                accepted = false;
        }
    }

    return NULL;
}

static void type_if( Cmd *father, Exp *exp, Stat *stat ) {
    Type *t1;
    Exp *eaux;

    t1 = get_exp( exp);
    if( is_array(t1) ) {
        fprintf(stderr, "error: if condition cannot be an array in line %d\n",
                    global_state->cur_line);
        accepted = false;
    }
    else {
        if( is_numeral(t1) && !is_float(t1) ) {
            Type *tbool;

            tbool = newtype(BOOL);
            eaux = asexp( exp, global_state->cur_line, tbool );
            father->cmd_if.exp = eaux;
        }
        else if( !is_bool(t1) ){
            fprintf(stderr, "error: if condition cannot be converted to"
                        " a boolean type in line %d\n", global_state->cur_line);
            accepted = false;
        }
    }
    type_stat( stat );
}

static void type_else( Stat *stat ) {
    if (stat)     type_stat( stat );
}


static void type_ifelse ( Cmd *father, Exp *exp, Stat *stat, Stat *stat2 ) {
    type_if( father, exp, stat );
    type_else( stat2 );
}

static void type_ret () {
}

static void get_retexp( Cmd *father, Exp *exp ) {
    Type *t1, *t2;
    char *name;
    name = global_state->cur_func_name;
    t1 = global_state->cur_func_type;
    if (!t1) {
        fprintf(stderr, "error: cannot return expression in function %s with"
                " no return type in line %d\n", name, global_state->cur_line);
        accepted = false;
    }
    t2 = get_exp( exp );
    if( compare_type(t1, t2)) return;
    // hora do show
    Exp *eaux;
    if( is_numeral(t1) && is_numeral(t2) ) {
        if( !compare_type(t1, t2) ) {
            // quero que exp tenha o tipo t1
            eaux = asexp( exp, global_state->cur_line, t1 );
            father->cmd_ret_exp.exp = eaux;
        }
    }
    else if( (is_bool(t1) && !is_array(t1)) && is_numeral(t2) ) {
        if( is_float(t2) ) {
            fprintf(stderr, "error: float is not compatible with %s return type"
                    " in line %d\n", name, global_state->cur_line );
            accepted = false;
        }
        else {
            eaux = asexp( exp, global_state->cur_line, t1 );
            father->cmd_ret_exp.exp = eaux;
        }
    }
    else if( is_numeral(t1) && (is_bool(t2) && !is_array(t2) ) ) {
        if( is_float(t1) ) {
            fprintf(stderr, "error: bool is not compatible with %s return type"
                    " in line %d\n", name, global_state->cur_line);
            accepted = false;
        }
        else {
            eaux = asexp( exp, global_state->cur_line, t1);
            father->cmd_ret_exp.exp = eaux;
        }
    }
    else if( !compare_type(t1, t2) ) {
        fprintf(stderr, "error: wrong return type for function %s in line %d\n",
                name, global_state->cur_line);
        accepted = false;
    }

}

static void type_while ( Cmd *father, Exp *exp, Stat *stat ) {
    Type *t1;

    t1 = get_exp(exp);
    // certificar de que esta single bool
    Exp *eaux;
    if( is_array(t1) ) {
        // nao pode dar certo
        fprintf(stderr, "error: while condition cannot be an array in line: %d\n",
                global_state->cur_line );
        accepted = false;
    }
    else {
        if( is_numeral(t1) && !is_float(t1) ) {
            Type *tbool;

            tbool = newtype(BOOL);
            eaux = asexp( exp, global_state->cur_line, tbool );
            father->cmd_while.exp = eaux;
        }
        else if( !is_bool(t1) ){
            fprintf(stderr, "error: while condition cannot be converted to boolean"
                    " type in line %d\n", global_state->cur_line );
            accepted = false;
        }
    }
    if(stat)     type_stat( stat );
}

static void type_print( Exp *exp ) {
    get_exp( exp);
}

static void type_cmd( Cmd *cmd ) {
    if (!cmd)
        return;
    switch ( cmd->tag ) {
        case IF:
            global_state->cur_line = cmd->cmd_if.line;
            type_if( cmd, cmd->cmd_if.exp, cmd->cmd_if.stat);
            type_cmd( cmd->cmd_if.next);
            break;
        case IFELSE:
            global_state->cur_line = cmd->cmd_ifelse.line;
            type_ifelse( cmd, cmd->cmd_ifelse.exp, cmd->cmd_ifelse.stat,
                            cmd->cmd_ifelse.stat2 );
            type_cmd( cmd->cmd_ifelse.next );
            break;
        case RET:
            global_state->cur_line = cmd->cmd_ret.line;
            type_ret();
            type_cmd( cmd->cmd_ret.next );
            break;
        case RETEXP:
            global_state->cur_line = cmd->cmd_ret_exp.line;
            get_retexp( cmd, cmd->cmd_ret_exp.exp);
            type_cmd( cmd->cmd_ret_exp.next );
            break;
        case WHILE:
            global_state->cur_line = cmd->cmd_while.line;
            type_while( cmd, cmd->cmd_while.exp, cmd->cmd_while.stat);
            type_cmd( cmd->cmd_while.next );
            break;
        case PRINT:
            global_state->cur_line = cmd->print.line;
            type_print( cmd->print.exp);
            type_cmd( cmd->print.next );
            break;
        case CALLCMD:
            global_state->cur_line = cmd->call.line;
            get_exp( cmd->call.call );
            type_cmd( cmd->call.next );
            break;
        case ATTCMD:
            global_state->cur_line = cmd->att.line;
            get_exp( cmd->att.att );
            type_cmd( cmd->att.next );
            break;
        case STAT:
            type_cmd( cmd->stat.next);
            break;
        default:
            fprintf(stderr, "Unknown command!\n");
            accepted = false;
    }
}

static void type_stat( Stat *stat ) {
    if (!stat) {
        return;
    }

    enter_scope();
    type_var( stat->vars);
    type_cmd( stat->cmds);
    leave_scope();
}

static void type_func( Func* f ) {
    if( f != NULL ) {
        insert_func(f);
        global_state->cur_func_type = f->type; 
        global_state->cur_func_name = f->name;
        enter_scope();
        type_params( f->param );
        if( f->stat )
            type_stat( f->stat );

        leave_scope();
        if( f->next )
            type_func( f->next );
    }
}

void type_defs(Def *def) {
    if (!def) {
        return;
    }

    switch (def->tag) {
        case DEFVAR:
            type_var( def->vars.vars );
            type_defs( def->vars.next );
            break;
        case DEFFUNC:
            type_func( def->funcs.funcs );
            type_defs( def->funcs.next );
            break;
    }
}

bool  type_tree() {
    type_defs(GLOBAL_TREE);
	return accepted;
}
