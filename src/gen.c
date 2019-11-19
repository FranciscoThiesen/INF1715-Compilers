#include "ast.h"
#include "symbols.h"
#include "typing.h"
#include "aux.h"
#include <stdio.h>
#include <stdbool.h>

#define GENTYPE(t) \
    gen_type(t);

static void gen_temporary_code(int expnum) {
    printf("%%t%d", expnum);
}

static int get_type_size(Type *t) {
    if (t->tag == SEQ)	
        return 8;

    switch (t->single.type) {
        case CHAR:
        case INT:
            return 4;
        case FLOAT:
            return 8;
        case BOOL:
            return 1;
        default:
            return 0;
    }
}

static void gen_printf_call(char *t, char *tfmt, int sz, int expnum) {
    printf("call i32 (i8*, ...) @printf( i8* getelementptr ([%d x i8],"
            "[%d x i8]*  @fmt%s, i32 0, i32 0), %s ", sz, sz, t, tfmt);
    gen_temporary_code(expnum);
    printf(")\n");
}

static Type *get_exp_type_internal(Exp *exp) {
    switch(exp->tag) {
        case VAR:
            return get_exp_type_internal(exp->binary.e1);
        case VARID:
            return exp->var.def->type;
        case EXPCHAR:
            return exp->expchar.type;
        case EXPINT:
            return exp->expint.type;
        case EXPFLOAT:
            return exp->expfloat.type;
        case EXPBOOL:
            return exp->expbool.type;
        case SUM:
        case SUB:
        case MUL:
        case DIV:
        case OR:
        case NEQ:
        case EQ:
        case GEQ:
        case LEQ:
        case L:
        case G:
        case AND:
            return exp->binary.exptype;
        case NOT:
        case MINUS:
            return exp->unary.exptype;
        case NEW:
            return exp->new.type;
        default:
            fprintf(stderr, "not implemented: %d\n", exp->tag);
            exit(-1);
    }
}

static void gen_native_type(Native_types type) {
    switch( type ) {
        case CHAR:
        case INT:
            printf("i32");
            break;
        case FLOAT:
            printf("double");
            break;
        case BOOL:
            printf("i8");
            break;
        default:
            break;
    }
}

static void gen_type(Type *type) {
    if (type == NULL) {
        printf("void");
        return;
    }

    if (type->tag == SINGLE) {
        gen_native_type(type->single.type);
    } else {
        gen_type(type->seq.next);
        printf("*");
    }

}

static void gen_pointer_reference(Type *t, int temp) {
    GENTYPE(t);
    printf("* ");
    gen_temporary_code(temp);
    printf("\n");
}

static int get_new_temporary(State *global_state) {
    return ++(global_state->temp_count);
}

static int copy_var_address(Type *type, char *name, bool is_global, 
        State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = getelementptr ");
    GENTYPE(type);
    printf(", ");
    GENTYPE(type);
    printf("* ");
    if(is_global) printf("@");
    else printf("%%");
    printf("%s", name);
    printf(", i32 0\n");
    return global_state->temp_count;
}

static void gen_load(Type *t, int address_temporary, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = load ");
    GENTYPE(t);
    printf(", ");
    gen_pointer_reference(t, address_temporary);
}

static void gen_store(Type* t, int address_temporary, int expnum) {
    printf("store ");
    GENTYPE(t); 
    printf(" ");
    gen_temporary_code(expnum);
    printf(", ");
    GENTYPE(t);
    printf("* ");
    gen_temporary_code(address_temporary);
    printf("\n");
}

static int gen_exp(Exp *exp, State *global_state);
static void gen_defs(Def *def, State *global_state, bool is_global);

static int gen_int(int eint, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = add i32 0, %d\n", eint);
    return global_state->temp_count;
}

static int gen_float(float efloat, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = fadd double %e, %e\n", 0.0, efloat);
    return global_state->temp_count;
}

static int gen_bool(bool ebool, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = add i8  0, %d\n", ebool);
    return global_state->temp_count;
}

static void gen_zero(Type *type) {
    if (type->tag == SEQ) {
        printf("null");
        return;
    }

    switch(type->single.type) {
        case CHAR:
        case BOOL:
        case INT:
            printf("0");
            break;
        case FLOAT:
            printf("%e", 0.0);
            break;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }
}

static int gen_exp_att(Exp *e1, Exp *e2, State *global_state) {
    int exp1num = gen_exp(e1, global_state);
    int exp2num = gen_exp(e2, global_state);
    Type *exp1type = get_exp_type_internal(e1);
    //Type *exp2type = get_exp_type_internal(e2);

    //Var *v = e1->var.def;
    //int varnum = copy_var_address(v->type, v->name, v->is_global, global_state);

    gen_store(exp1type, exp1num, exp2num);

    return global_state->temp_count;
}

static int gen_exp_varid(Exp *exp, State *global_state) {
    Var *v = exp->var.def;
    int address_temporary = copy_var_address(v->type, v->name, v->is_global, global_state);
    //gen_load(v->type, address_temporary, global_state);

    return global_state->temp_count;
}

static int gen_exp_var(Exp *evar, Exp *eind, State *global_state) {
    Type *vartype = get_exp_type_internal(evar);
    int varnum = gen_exp(evar, global_state);
    int indnum = gen_exp(eind, global_state);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = getelementptr ");
    GENTYPE(vartype);
    printf(", ");
    GENTYPE(vartype);
    printf("* ");
    gen_temporary_code(varnum);
    printf(", i32 ");
    gen_temporary_code(indnum);
    printf("\n");

    return global_state->temp_count;
}

static int gen_exp_new(Exp *exp, Type *type, State *global_state) {
    int expnum = gen_exp(exp, global_state);
    int tsize = get_type_size(type);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = sext i32 ");
    gen_temporary_code(expnum);
    printf(" to i64\n");
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = mul i64 ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(", %d\n", tsize);
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = call i8* @malloc(i64 ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(")\n");
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = bitcast i8 * ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(" to ");
    GENTYPE(type);
    printf("\n");

    return global_state->temp_count;
}

static void gen_ret() {
    printf("ret void\n");
}

static void gen_retexp(Exp *exp, State *global_state) {
    int numexp = gen_exp(exp, global_state);

    printf("ret ");
    GENTYPE(global_state->cur_func_type);
    printf(" ");
    gen_temporary_code(numexp);
    printf("\n");

}

static void gen_ext_i1_to_i8(State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    printf("zext i1 ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(" to i8\n");
}

static int gen_binary_exp(Exp *e1, Exp *e2, Exp_type etype, State *global_state) {
    Type *te1 = get_exp_type_internal(e1);
    int e1num = gen_exp(e1, global_state);
    int e2num = gen_exp(e2, global_state);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    switch(etype) {
        case SUM:
            if (te1->single.type == FLOAT)
                printf("fadd ");
            else
                printf("add nsw ");

            break;
        case SUB:
            if (te1->single.type == FLOAT)
                printf("fsub ");
            else
                printf("sub nsw ");

            break;
        case MUL:
            if (te1->single.type == FLOAT)
                printf("fmul ");
            else
                printf("mul nsw ");

            break;
        case DIV:
            if (te1->single.type == FLOAT)
                printf("fdiv ");
            else
                printf("sdiv ");
            break;
        case OR:
            printf("or ");
            break;
        case AND:
            printf("and ");
            break;
        case NEQ:
            if (te1->single.type == FLOAT)
                printf("fcmp one ");
            else
                printf("icmp ne ");
            break;
        case EQ:
            if (te1->single.type == FLOAT)
                printf("fcmp oeq ");
            else
                printf("icmp eq ");
            break;
        case G:
            if (te1->single.type == FLOAT)
                printf("fcmp ogt ");
            else
                printf("icmp sgt ");
            break;
        case L:
            if (te1->single.type == FLOAT)
                printf("fcmp olt ");
            else
                printf("icmp slt "); 
            break;
        case GEQ:
            if (te1->single.type == FLOAT)
                printf("fcmp oge ");
            else
                printf("icmp sge "); 
            break;
        case LEQ:
            if (te1->single.type == FLOAT)
                printf("fcmp ole ");
            else
                printf("icmp sle "); 
            break;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }

    GENTYPE(te1);

    printf(" ");
    gen_temporary_code(e1num);
    printf(", ");
    gen_temporary_code(e2num);
    printf("\n");

    switch(etype) {
        case NEQ:
        case EQ:
        case G:
        case L:
        case GEQ:
        case LEQ:
            gen_ext_i1_to_i8(global_state);
            break;
        default:
            //noop
            break;
    }

    return global_state->temp_count;
}

static int gen_unary_type(Exp *e1, Exp_type etype, State *global_state) {
    int e1num = gen_exp(e1, global_state);

    Type *te1 = get_exp_type_internal(e1);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    switch(etype) {
        case NOT:
            printf("icmp ne i8 ");
            gen_temporary_code(e1num);
            printf(", 0\n");
            gen_temporary_code(get_new_temporary(global_state));
            printf(" = ");
            printf("xor i1 ");
            gen_temporary_code(global_state->temp_count - 1);
            printf(", true\n");
            gen_ext_i1_to_i8(global_state);
            break;
        case MINUS:
            if( te1->single.type == FLOAT ) printf("f");

            printf("sub ");
            GENTYPE(te1);
            printf(" 0, ");
            gen_temporary_code(global_state->temp_count - 1);
            printf("\n");
            break;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }

    return global_state->temp_count;
}

static int gen_exp(Exp *exp, State *global_state) {
    if( exp == NULL ) {
        fprintf(stderr, "error\n");
        exit(-1);
    }

    switch( exp->tag ) {
        case DIV:
        case MUL:
        case SUB:
        case SUM:
        case OR:
        case NEQ:
        case EQ:
        case GEQ:
        case LEQ:
        case L:
        case G:
        case AND:
            return gen_binary_exp(exp->binary.e1, exp->binary.e2, exp->tag, global_state);
        case NOT:
        case MINUS:
            return gen_unary_type(exp->unary.exp, exp->tag, global_state);
        case EXPATT:
            return gen_exp_att(exp->binary.e1, exp->binary.e2, global_state);
        case EXPINT:
            return gen_int(exp->expint.i, global_state);
        case EXPFLOAT:
            return gen_float(exp->expfloat.f, global_state);
        case EXPCHAR:
            return gen_int(exp->expchar.c, global_state);
        case EXPBOOL:
            return gen_bool(exp->expbool.b, global_state);
        case VARID:
            return gen_exp_varid(exp, global_state);
        case VAR:
            return gen_exp_var(exp->binary.e1, exp->binary.e2, global_state);
        case NEW:
            return gen_exp_new(exp->new.exp, exp->new.type, global_state);
        default:
            fprintf(stderr, "not implemented: %d\n", exp->tag);
            exit(-1);
    }
}

static void gen_print(Exp *exp, State *global_state) {
    int expnum = gen_exp(exp, global_state);
    Type *exptype = get_exp_type_internal(exp);

    if (exptype->tag == SINGLE) {
        switch(exptype->single.type) {
            case CHAR:
                gen_printf_call("char", "i32", 4, expnum);
                break;
            case INT:
                gen_printf_call("int", "i32", 4, expnum);
                break;
            case FLOAT:
                gen_printf_call("float", "double", 6, expnum);
                break;
            case BOOL:
                gen_printf_call("bool", "i8", 6, expnum);
                break;
            default:
                fprintf(stderr, "not implemented %d\n", exptype->single.type);
                exit(-1);
        }
    } else {
        fprintf(stderr, "not implemented\n");
        exit(-1);
    }
}

static void gen_cmd( Cmd *cmd, State *global_state ) {
    if (!cmd)
        return;

    switch ( cmd->tag ) {
        case PRINT:
            gen_print( cmd->print.exp, global_state);
            gen_cmd( cmd->print.next, global_state);
            break;
        case ATTCMD:
            gen_exp(cmd->att.att, global_state );
            gen_cmd(cmd->att.next, global_state);
            break;
        case RET:
            gen_ret();
            gen_cmd(cmd->cmd_ret.next, global_state);
            break;
        case RETEXP:
            global_state->cur_line = cmd->cmd_ret_exp.line;
            gen_retexp(cmd->cmd_ret_exp.exp, global_state);
            gen_cmd(cmd->cmd_ret_exp.next, global_state);
            break;
        default:
            fprintf(stderr, "not implemented: %d\n", cmd->tag);
            exit(-1);
    }

}

static void gen_stat(Stat *stat, State *global_state) {
    if (!stat) {
        return;
    }

    gen_defs(stat->vars, global_state, false);
    gen_cmd(stat->cmds, global_state);
}

static void gen_global_var(Def *var) {
    Var *v = var->var.def;

    printf("@%s = global ", v->name);
    GENTYPE(v->type);
    printf(" ");
    gen_zero(v->type);
    printf("\n");
    v->is_global = true;
}

static void gen_local_var(Def *var) {
    Var *v = var->var.def;

    printf("%%%s = alloca ", v->name);
    GENTYPE(v->type);
    printf("\n");
}

static void gen_params(Def *params, State *global_state) {
    if( params != NULL ) {
        fprintf(stderr, "not implemented\n");
        exit(-1);
    }
}

static void gen_func(Def *dfunc, State *global_state) {
    Func *f;

    if (dfunc == NULL)
        return;

    f = dfunc->func.def;
    if (f == NULL)
        return;

    gen_params(f->param, global_state );

    global_state->cur_line = f->line;

    global_state->cur_func_type = f->type; 
    global_state->cur_func_name = f->name;

    printf("define ");
    GENTYPE(f->type);
    printf(" @%s() {\n", f->name);

    if (f->stat)
        gen_stat(f->stat, global_state);

    printf("}\n");

}

static void gen_defs(Def *def, State *global_state, bool is_global) {
    if (!def)
        return;

    switch (def->tag) {
        case DEFVAR:
            is_global ? gen_global_var(def) : gen_local_var(def);
            gen_defs(def->var.next, global_state, is_global);
            break;
        case DEFFUNC:
            gen_func(def, global_state);
            gen_defs(def->func.next, global_state, is_global);
            break;
    }
}

void gen_code(State *global_state) {
    printf("@fmtint = internal constant [4 x i8] c\"%%d\\0A\\00\"\n");
    printf("@fmtfloat = internal constant [6 x i8] c\"%%.7f\\0A\\00\"\n");
    printf("@fmtbool = internal constant [6 x i8] c\"%%hhx\\0A\\00\"\n");
    printf("@fmtchar = internal constant [4 x i8] c\"%%c\\0A\\00\"\n");
    printf("declare i32 @printf(i8*, ...)\n");
    printf("declare noalias i8* @malloc(i64)\n");
    gen_defs(GLOBAL_TREE, global_state, true);
}
